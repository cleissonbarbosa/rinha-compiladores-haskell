#include <fcntl.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#define DIM 14
#define Q16_SCALE 32767.0f
#define RINHA_SCALE 4096.0f
#define IVF_MAGIC "RIVF2026"

typedef struct {
    size_t n_vecs;
    uint32_t n_clusters;
    uint32_t nprobe;
    const int16_t *dims;
    const uint8_t *labels;
    const float *centroids;
    const uint32_t *boundaries;
    const void *labels_map;
    const void *vectors_map;
    const void *ivf_map;
    size_t labels_len;
    size_t vectors_len;
    size_t ivf_len;
    int loaded;
} FastIndex;

static FastIndex g_index;

static const void *map_file_readonly(const char *path, size_t *out_len) {
    int fd = open(path, O_RDONLY);
    if (fd < 0) return NULL;

    struct stat st;
    if (fstat(fd, &st) != 0 || st.st_size <= 0) {
        close(fd);
        return NULL;
    }

    void *ptr = mmap(NULL, (size_t)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    if (ptr == MAP_FAILED) return NULL;

    *out_len = (size_t)st.st_size;
    return ptr;
}

static uint32_t u32le(const unsigned char *p) {
    return (uint32_t)p[0] |
           ((uint32_t)p[1] << 8) |
           ((uint32_t)p[2] << 16) |
           ((uint32_t)p[3] << 24);
}

static int load_index(const char *resources_dir) {
    if (g_index.loaded) return 1;

    char path[512];
    size_t labels_len = 0, vectors_len = 0, ivf_len = 0;

    snprintf(path, sizeof(path), "%s/labels.bin", resources_dir);
    const uint8_t *labels = map_file_readonly(path, &labels_len);
    if (!labels) return 0;

    snprintf(path, sizeof(path), "%s/vectors.bin", resources_dir);
    const int16_t *vectors = map_file_readonly(path, &vectors_len);
    if (!vectors) return 0;
    if (vectors_len != labels_len * DIM * sizeof(int16_t)) return 0;

    snprintf(path, sizeof(path), "%s/ivf.bin", resources_dir);
    const unsigned char *ivf = map_file_readonly(path, &ivf_len);
    if (!ivf || ivf_len < 28 || memcmp(ivf, IVF_MAGIC, 8) != 0) return 0;

    uint32_t hdr_dim = u32le(ivf + 8);
    uint32_t n_clusters = u32le(ivf + 12);
    uint32_t nprobe = u32le(ivf + 16);
    uint32_t index_n = u32le(ivf + 20);
    if (hdr_dim != DIM || index_n != labels_len || n_clusters == 0) return 0;

    size_t off = 28;
    size_t centroid_bytes = (size_t)n_clusters * DIM * sizeof(float);
    size_t radii_bytes = (size_t)n_clusters * sizeof(float);
    size_t boundary_bytes = (size_t)(n_clusters + 1) * sizeof(uint32_t);
    if (off + centroid_bytes + radii_bytes + boundary_bytes != ivf_len) return 0;

    g_index.n_vecs = labels_len;
    g_index.n_clusters = n_clusters;
    g_index.nprobe = nprobe == 0 ? 1 : nprobe;
    g_index.dims = vectors;
    g_index.labels = labels;
    g_index.centroids = (const float *)(const void *)(ivf + off);
    g_index.boundaries = (const uint32_t *)(const void *)(ivf + off + centroid_bytes + radii_bytes);
    g_index.labels_map = labels;
    g_index.vectors_map = vectors;
    g_index.ivf_map = ivf;
    g_index.labels_len = labels_len;
    g_index.vectors_len = vectors_len;
    g_index.ivf_len = ivf_len;
    g_index.loaded = 1;
    return 1;
}

int rinha_fast_ivf_warmup(const char *resources_dir) {
    return load_index(resources_dir) ? 0 : -1;
}

int rinha_fast_ivf_score(const int *query_rinha, const char *resources_dir, int requested_probe) {
    if (!load_index(resources_dir)) return 5;

    const FastIndex *idx = &g_index;
    int probe = requested_probe > 0 ? requested_probe : (int)idx->nprobe;
    if (probe < 1) probe = 1;
    if (probe > 32) probe = 32;

    float query[DIM];
    for (int k = 0; k < DIM; k++) {
        query[k] = ((float)query_rinha[k] * Q16_SCALE) / RINHA_SCALE;
    }

    float best_dist[32];
    int best_cluster[32];
    for (int i = 0; i < probe; i++) {
        best_dist[i] = INFINITY;
        best_cluster[i] = -1;
    }

    for (uint32_t c = 0; c < idx->n_clusters; c++) {
        const float *centroid = idx->centroids + (size_t)c * DIM;
        float dist = 0.0f;
        for (int k = 0; k < DIM; k++) {
            float diff = query[k] - centroid[k];
            dist += diff * diff;
        }
        if (dist >= best_dist[probe - 1]) continue;

        int pos = probe - 1;
        while (pos > 0 && best_dist[pos - 1] > dist) {
            best_dist[pos] = best_dist[pos - 1];
            best_cluster[pos] = best_cluster[pos - 1];
            pos--;
        }
        best_dist[pos] = dist;
        best_cluster[pos] = (int)c;
    }

    float top_dist[5];
    uint32_t top_id[5];
    int count = 0;
    for (int i = 0; i < 5; i++) {
        top_dist[i] = INFINITY;
        top_id[i] = 0;
    }

    for (int pi = 0; pi < probe; pi++) {
        int c = best_cluster[pi];
        if (c < 0) continue;

        uint32_t start = idx->boundaries[c];
        uint32_t end = idx->boundaries[c + 1];
        for (uint32_t v = start; v < end; v++) {
            float dist = 0.0f;
            for (int k = 0; k < DIM; k++) {
                float ref = (float)idx->dims[(size_t)k * idx->n_vecs + v];
                float diff = query[k] - ref;
                dist += diff * diff;
            }

            if (count < 5) {
                int pos = count;
                while (pos > 0 && top_dist[pos - 1] > dist) {
                    top_dist[pos] = top_dist[pos - 1];
                    top_id[pos] = top_id[pos - 1];
                    pos--;
                }
                top_dist[pos] = dist;
                top_id[pos] = v;
                count++;
            } else if (dist < top_dist[4]) {
                int pos = 4;
                while (pos > 0 && top_dist[pos - 1] > dist) {
                    top_dist[pos] = top_dist[pos - 1];
                    top_id[pos] = top_id[pos - 1];
                    pos--;
                }
                top_dist[pos] = dist;
                top_id[pos] = v;
            }
        }
    }

    int fraud = 0;
    for (int i = 0; i < count; i++) {
        fraud += idx->labels[top_id[i]] != 0;
    }
    return fraud;
}

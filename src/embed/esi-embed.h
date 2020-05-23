#pragma once
#ifdef __cplusplus
extern "C" {
#endif
#include <stdint.h>

typedef struct {
  void *obj;
} embed_model_t;

embed_model_t* load_embed_model(char *filepath);
double* embed_model_run(embed_model_t* m, double* mel_sg, size_t n_frames, size_t *n_out);
void embed_model_destroy(embed_model_t* m);

#ifdef __cplusplus
}
#endif

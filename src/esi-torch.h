#pragma once
#ifdef __cplusplus
extern "C" {
#endif

struct _torch_module;
typedef struct _torch_module torch_module_t;

torch_module_t* load_torch_module(char *filepath);
void torch_module_forward(torch_module_t* m, double *x);

#ifdef __cplusplus
}
#endif

#include "emacs-module.h"
#include "sndfile.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <complex.h>

#include "esi-prep.h"
#include "esi-embed.h"

int plugin_is_GPL_compatible;
const char *esi_embed_core_version = "0.0.2";

static emacs_value Fesi_embed_core_version(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  return env->make_string(env, esi_embed_core_version, strlen(esi_embed_core_version));
}

static emacs_value Fload_model(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  ptrdiff_t buffer_size;
  env->copy_string_contents(env, args[0], NULL, &buffer_size);

  char *filepath = malloc(buffer_size);
  env->copy_string_contents(env, args[0], filepath, &buffer_size);

  embed_model_t* m = load_embed_model(filepath);
  // TODO: Add a finalizer
  return env->make_user_ptr(env, NULL, (void *)m);
}

// Run the embedding model on samples and return fixed length vector
// - model-user-pointer
// - samples
// - sample-rate
static emacs_value Frun_model(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  embed_model_t *m = env->get_user_ptr(env, args[0]);
  size_t sr = env->extract_integer(env, args[2]);

  // NOTE: These values are for the currently used model at 16000
  double mel_win = 25.0 / 1000.0;
  double mel_hop = 10.0 / 1000.0;
  size_t n_fft = floor(sr * mel_win);
  size_t hop_length = floor(sr * mel_hop);
  size_t n_mels = 40;

  size_t n_samples = env->vec_size(env, args[1]);
  double *samples = malloc(sizeof(double) * n_samples);
  for (size_t i = 0; i < n_samples; i++) {
    samples[i] = env->extract_float(env, env->vec_get(env, args[1], i));
  }

  size_t n_cols;
  double *msg_matrix = melspectrogram(samples, n_samples, sr, n_fft, hop_length, n_mels, &n_cols);

  size_t embedding_size;
  double* embedding = embed_model_run(m, msg_matrix, n_cols, &embedding_size);

  emacs_value vector = make_vector(env, embedding_size, 0);
  for (size_t i = 0; i < embedding_size; i++) {
    env->vec_set(env, vector, i, env->make_float(env, embedding[i]));
  }

  free(samples);
  free(msg_matrix);
  free(embedding);
  return vector;
}

static void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};

  env->funcall(env, Qprovide, 1, args);
}

static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);
  emacs_value args[] = {Qsym, Sfun};
  env->funcall(env, Qfset, 2, args);
}

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  emacs_value version_fn = env->make_function(env, 0, 0, Fesi_embed_core_version, "Return version of esi-embed-core.", NULL);
  bind_function(env, "esi-embed-core-version", version_fn);

  emacs_value load_model_fn = env->make_function(env, 1, 1,
                                                Fload_model,
                                                "Load speech embedding model.\n\n"
                                                "\(fn file-path)",
                                                NULL);
  bind_function(env, "esi-embed-core--load-model", load_model_fn);

  emacs_value run_model_fn = env->make_function(env, 3, 3,
                                               Frun_model,
                                               "Run embedding model on given samples.\n\n"
                                               "\(fn model-user-pointer samples sample-rate)",
                                               NULL);
  bind_function(env, "esi-embed-core--run-model", run_model_fn);

  provide(env, "esi-embed-core");

  return 0;
}

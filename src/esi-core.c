#include "emacs-module.h"
#include "sndfile.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "esi-io.h"
#include "esi-prep.h"

int plugin_is_GPL_compatible;
const char *esi_core_version = "0.0.1";

static emacs_value Fesi_core_version(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  return env->make_string(env, esi_core_version, strlen(esi_core_version));
}

static emacs_value make_vector(emacs_env *env, int len, double init) {
  emacs_value Fmake_vector = env->intern(env, "make-vector");
  emacs_value args[] = { env->make_integer(env, len), env->make_float(env, init) };
  return env->funcall(env, Fmake_vector, 2, args);
}

static emacs_value Fwav_to_samples(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  ptrdiff_t buffer_size;
  env->copy_string_contents(env, args[0], NULL, &buffer_size);

  char *buffer = malloc(buffer_size);
  env->copy_string_contents(env, args[0], buffer, &buffer_size);

  sf_count_t n_samples;
  float *samples = samples_from_buffer(buffer, buffer_size, &n_samples);

  emacs_value vector = make_vector(env, n_samples, 0);
  for (size_t i = 0; i < n_samples; i++) {
    env->vec_set(env, vector, i, env->make_float(env, samples[i]));
  }

  free(samples);
  free(buffer);
  return vector;
}

// Create spectrogram matrix (elisp vectors) using the following arguments:
// - samples (a vector)
// - n-fft
// - hop-length
// - power (defaults to 1)
static emacs_value Fsamples_to_spectrogram(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  size_t n_fft = env->extract_integer(env, args[1]);
  size_t hop_length = env->extract_integer(env, args[2]);
  size_t power = n == 3 ? 1 : env->extract_integer(env, args[3]);

  size_t n_samples = env->vec_size(env, args[0]);
  float* samples = malloc(sizeof(float) * n_samples);
  for (size_t i = 0; i < n_samples; i++) {
    samples[i] = env->extract_float(env, env->vec_get(env, args[0], i));
  }

  size_t n_rows, n_cols;
  double* sg_matrix = spectrogram(samples, n_samples, n_fft, hop_length, power, &n_rows, &n_cols);
  emacs_value matrix = make_vector(env, n_rows, 0);
  for (size_t i = 0; i < n_rows; i++) {
    emacs_value vector = make_vector(env, n_cols, 0);

    // sg_matrix is row major
    for (size_t j = 0; j < n_cols; j++) {
      env->vec_set(env, vector, j, env->make_float(env, sg_matrix[(i * n_cols) + j]));
    }

    env->vec_set(env, matrix, i, vector);
  }

  free(samples);
  free(sg_matrix);

  return matrix;
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

  emacs_value version_fn = env->make_function(env, 0, 0, Fesi_core_version, "Return version of esi-core.", NULL);
  bind_function(env, "esi-core-version", version_fn);

  emacs_value sample_fn = env->make_function(env, 1, 1, Fwav_to_samples, "Read and return vector of samples from wav bytes", NULL);
  bind_function(env, "esi-core-wav-to-samples", sample_fn);

  emacs_value spectrogram_fn = env->make_function(env, 3, 4,
                                                 Fsamples_to_spectrogram,
                                                 "Calculate spectrogram for given samples.\n\n"
                                                 "\(fn samples-vector n-fft hop-length &optional power)",
                                                 NULL);
  bind_function(env, "esi-core-samples-to-spectrogram", spectrogram_fn);

  provide(env, "esi-core");

  return 0;
}

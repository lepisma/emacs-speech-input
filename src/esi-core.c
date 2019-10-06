#include "emacs-module.h"
#include "sndfile.h"
#include <stdlib.h>
#include <string.h>

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
  for (int i = 0; i < n_samples; i++) {
    env->vec_set(env, vector, i, env->make_float(env, samples[i]));
  }

  free(samples);
  free(buffer);
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

  emacs_value v_fun = env->make_function(env, 0, 0, Fesi_core_version, "Return version of esi-core.", NULL);
  bind_function(env, "esi-core-version", v_fun);

  emacs_value samp_fun = env->make_function(env, 1, 1, Fwav_to_samples, "Read and return vector of samples from wav bytes", NULL);
  bind_function(env, "esi-core-wav-to-samples", samp_fun);

  provide(env, "esi-core");

  return 0;
}

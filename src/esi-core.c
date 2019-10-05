#include "emacs-module.h"
#include "sndfile.h"
#include <stdlib.h>
#include <string.h>

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

typedef struct {
  sf_count_t offset, length;
  char *data;
} VIO_DATA;

static sf_count_t vfget_filelen(void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;
  return vf->length;
}

static sf_count_t vfseek(sf_count_t offset, int whence, void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;

  switch (whence) {
  case SEEK_SET:
    vf->offset = offset;
    break;

  case SEEK_CUR:
    vf->offset = vf->offset + offset;
    break;

  case SEEK_END:
    vf->offset = vf->length + offset;
    break;

  default:
    break;
  };

  return vf->offset;
}

static sf_count_t vfread(void *ptr, sf_count_t count, void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;

  if (vf->offset + count > vf->length)
    count = vf->length - vf->offset;

  memcpy(ptr, vf->data + vf->offset, count);
  vf->offset += count;

  return count;
}

static sf_count_t vftell(void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;
  return vf->offset;
}

static emacs_value Fwav_to_samples(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  ptrdiff_t buffer_size;
  env->copy_string_contents(env, args[0], NULL, &buffer_size);

  char *buffer = malloc(buffer_size);
  env->copy_string_contents(env, args[0], buffer, &buffer_size);

  VIO_DATA vio_data;
  SF_VIRTUAL_IO vio;

  vio.get_filelen = vfget_filelen;
  vio.seek = vfseek;
  vio.read = vfread;
  vio.tell = vftell;

  vio_data.offset = 0;
  vio_data.length = buffer_size - 1;
  vio_data.data = buffer;

  SNDFILE *sfile;
  SF_INFO file_info;

  sfile = sf_open_virtual(&vio, SFM_READ, &file_info, &vio_data);

  sf_count_t frames_count = file_info.frames * file_info.channels;
  float *frames = malloc(sizeof(float) * frames_count);
  sf_readf_float(sfile, frames, frames_count);

  emacs_value samples = make_vector(env, frames_count, 0);
  for (int i = 0; i < file_info.frames; i++) {
    env->vec_set(env, samples, i, env->make_float(env, frames[i * file_info.channels]));
  }

  sf_close(sfile);
  free(frames);
  free(buffer);

  return samples;
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

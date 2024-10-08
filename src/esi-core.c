#include "emacs-module.h"
#include "sndfile.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <complex.h>

#include "esi-io.h"
#include "esi-prep.h"

int plugin_is_GPL_compatible;
const char *esi_core_version = "0.0.2";

static emacs_value Fesi_core_version(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  return env->make_string(env, esi_core_version, strlen(esi_core_version));
}

static emacs_value make_vector(emacs_env *env, int len, double init) {
  emacs_value Fmake_vector = env->intern(env, "make-vector");
  emacs_value args[] = { env->make_integer(env, len), env->make_float(env, init) };
  return env->funcall(env, Fmake_vector, 2, args);
}

// Return an elisp matrix from a given row major float matrix
emacs_value make_matrix(emacs_env *env, double* matrix, size_t n_rows, size_t n_cols) {
  emacs_value output = make_vector(env, n_rows, 0);
  for (size_t i = 0; i < n_rows; i++) {
    emacs_value row = make_vector(env, n_cols, 0);
    for (size_t j = 0; j < n_cols; j++) {
      env->vec_set(env, row, j, env->make_float(env, matrix[(i * n_cols) + j]));
    }

    env->vec_set(env, output, i, row);
  }

  return output;
}

static emacs_value Fwav_to_samples(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  ptrdiff_t buffer_size;
  env->copy_string_contents(env, args[0], NULL, &buffer_size);

  char *buffer = malloc(buffer_size);
  env->copy_string_contents(env, args[0], buffer, &buffer_size);

  sf_count_t n_samples;
  double *samples = samples_from_buffer(buffer, buffer_size, &n_samples);

  emacs_value vector = make_vector(env, n_samples, 0);
  for (size_t i = 0; i < n_samples; i++) {
    env->vec_set(env, vector, i, env->make_float(env, samples[i]));
  }

  free(samples);
  free(buffer);
  return vector;
}

// Does raw rfft using the following arguments:
// - samples (a vector)
static emacs_value Frfft(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  size_t n_samples = env->vec_size(env, args[0]);

  double *samples = fftw_malloc(sizeof(double) * n_samples);
  for (size_t i = 0; i < n_samples; i++) {
    samples[i] = env->extract_float(env, env->vec_get(env, args[0], i));
  }

  size_t n_output;
  fftw_complex *fft_output = rfft(samples, n_samples, &n_output);
  emacs_value vector = make_vector(env, n_output, 0);

  for (size_t i = 0; i < n_output; i++) {
    emacs_value complex_number = make_vector(env, 2, 0);
    env->vec_set(env, complex_number, 0, env->make_float(env, creal(fft_output[i])));
    env->vec_set(env, complex_number, 1, env->make_float(env, cimag(fft_output[i])));
    env->vec_set(env, vector, i, complex_number);
  }

  fftw_free(fft_output);
  fftw_free(samples);
  return vector;
}

// Create stft matrix (elisp vector based) using the following arguments:
// - samples (a vector)
// - n-fft
// - hop-length
static emacs_value Fstft(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  size_t n_fft = env->extract_integer(env, args[1]);
  size_t hop_length = env->extract_integer(env, args[2]);

  size_t n_samples = env->vec_size(env, args[0]);
  double* samples = malloc(sizeof(double) * n_samples);
  for (size_t i = 0; i < n_samples; i++) {
    samples[i] = env->extract_float(env, env->vec_get(env, args[0], i));
  }

  size_t n_rows, n_cols;
  fftw_complex* stft_matrix = stft(samples, n_samples, n_fft, hop_length, &n_rows, &n_cols);

  emacs_value matrix = make_vector(env, n_rows, 0);
  for (size_t i = 0; i < n_rows; i++) {
    emacs_value vector = make_vector(env, n_cols, 0);

    // stft_matrix is row major
    for (size_t j = 0; j < n_cols; j++) {
      emacs_value complex_number = make_vector(env, 2, 0);
      env->vec_set(env, complex_number, 0, env->make_float(env, creal(stft_matrix[(i * n_cols) + j])));
      env->vec_set(env, complex_number, 1, env->make_float(env, cimag(stft_matrix[(i * n_cols) + j])));
      env->vec_set(env, vector, j, complex_number);
    }

    env->vec_set(env, matrix, i, vector);
  }

  free(samples);
  free(stft_matrix);

  return matrix;
}

// Create spectrogram matrix (elisp vectors) using the following arguments:
// - samples (a vector)
// - n-fft
// - hop-length
// - power (defaults to 1)
static emacs_value Fspectrogram(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  size_t n_fft = env->extract_integer(env, args[1]);
  size_t hop_length = env->extract_integer(env, args[2]);
  size_t power = n == 3 ? 1 : env->extract_integer(env, args[3]);

  size_t n_samples = env->vec_size(env, args[0]);
  double* samples = malloc(sizeof(double) * n_samples);
  for (size_t i = 0; i < n_samples; i++) {
    samples[i] = env->extract_float(env, env->vec_get(env, args[0], i));
  }

  size_t n_rows, n_cols;
  double* sg_matrix = spectrogram(samples, n_samples, n_fft, hop_length, power, &n_rows, &n_cols);
  emacs_value matrix = make_matrix(env, sg_matrix, n_rows, n_cols);
  free(samples);
  free(sg_matrix);
  return matrix;
}

// Create mel-spectrogram matrix using the following arguments:
// - samples (a vector)
// - sample rate
// - n-fft
// - hop-length
// - n-mels
static emacs_value Fmel_spectrogram(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  size_t sr = env->extract_integer(env, args[1]);
  size_t n_fft = env->extract_integer(env, args[2]);
  size_t hop_length = env->extract_integer(env, args[3]);
  size_t n_mels = env->extract_integer(env, args[4]);

  size_t n_samples = env->vec_size(env, args[0]);
  double *samples = malloc(sizeof(double) * n_samples);
  for (size_t i = 0; i < n_samples; i++) {
    samples[i] = env->extract_float(env, env->vec_get(env, args[0], i));
  }

  size_t n_cols;
  double *msg_matrix = melspectrogram(samples, n_samples, sr, n_fft, hop_length, n_mels, &n_cols);
  emacs_value matrix = make_matrix(env, msg_matrix, n_mels, n_cols);
  free(samples);
  free(msg_matrix);
  return matrix;
}

// Create mel filterbank matrix (elisp vectors) using the following arguments:
// - sample rate
// - n-fft
// - n-mels
static emacs_value Fmel_filter(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  size_t sr = env->extract_integer(env, args[0]);
  size_t n_fft = env->extract_integer(env, args[1]);
  size_t n_mels = env->extract_integer(env, args[2]);

  double* filterbank = mel_filter(sr, n_fft, n_mels);
  emacs_value matrix = make_matrix(env, filterbank, n_mels, 1 + floor(n_fft / 2));
  free(filterbank);
  return matrix;
}

static void fin_instream(void *instream_ut) {
  struct SoundIoInStream *instream = (struct SoundIoInStream*)instream_ut;
  stop_background_recording(instream);
}

// Start background audio recording with a max buffer limit. Return 1 in case of error.
// Arguments:
// - sample-rate
// - buffer-duration (in seconds)
static emacs_value Fstart_background_recording(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  size_t sr = env->extract_integer(env, args[0]);
  size_t buffer_duration_seconds = env->extract_integer(env, args[1]);

  struct SoundIoInStream *instream = start_background_recording(sr, buffer_duration_seconds);

  if (instream) {
    return env->make_user_ptr(env, fin_instream, instream);
  } else {
    emacs_value nil = env->intern(env, "nil");
    env->non_local_exit_signal(env, env->intern(env, "audio-input-stream-error"), nil);
    return nil;
  }
}

// Return current value of provided recording context
static emacs_value Fread_background_recording_buffer(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  struct SoundIoInStream *instream = (struct SoundIoInStream*)env->get_user_ptr(env, args[0]);

  size_t output_size;
  char* output = read_background_recording(instream, &output_size);

  return env->make_string(env, output, output_size);
}

// Stop the ongoing recording altogether and return buffer
static emacs_value Fstop_background_recording(emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  struct SoundIoInStream *instream = (struct SoundIoInStream*)(env->get_user_ptr(env, args[0]));

  emacs_value nil = env->intern(env, "nil");
  if (!instream) {
    // We put NULL in the value once a user pointer is finished
    env->non_local_exit_signal(env, env->intern(env, "audio-input-stream-finished"), nil);
    return nil;
  }

  // Check if we have valid user pointer value
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
    stop_background_recording(instream);
    env->set_user_ptr(env, args[0], NULL);
    env->set_user_finalizer(env, args[0], NULL);
  }

  return nil;
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

  emacs_value sample_fn = env->make_function(env, 1, 1, Fwav_to_samples,
                                            "Read and return vector of samples from wav bytes.\n\n"
                                            "\(fn wav-bytes)",
                                            NULL);
  bind_function(env, "esi-core-wav-to-samples", sample_fn);

  emacs_value rfft_fn = env->make_function(env, 1, 1,
                                         Frfft,
                                         "Calculate fft for given samples.\n\n"
                                         "\(fn samples-vector)",
                                         NULL);
  bind_function(env, "esi-core--rfft", rfft_fn);

  emacs_value stft_fn = env->make_function(env, 3, 3,
                                          Fstft,
                                          "Calculate stft for given samples.\n\n"
                                          "\(fn samples-vector n-fft hop-length)",
                                          NULL);
  bind_function(env, "esi-core--stft", stft_fn);

  emacs_value spectrogram_fn = env->make_function(env, 3, 4,
                                                 Fspectrogram,
                                                 "Calculate spectrogram for given samples.\n\n"
                                                 "\(fn samples-vector n-fft hop-length &optional power)",
                                                 NULL);
  bind_function(env, "esi-core--spectrogram", spectrogram_fn);

  emacs_value melfb_fn = env->make_function(env, 3, 3,
                                           Fmel_filter,
                                           "Calculate mel filterbank matrix.\n\n"
                                           "\(fn sample-rate n-fft n-mels)",
                                           NULL);
  bind_function(env, "esi-core--mel-filter", melfb_fn);

  emacs_value mel_spectrogram_fn = env->make_function(env, 5, 5,
                                                     Fmel_spectrogram,
                                                     "Calculate mel-spectrogram for given samples.\n\n"
                                                     "\(fn samples-vector sample-rate n-fft hop-length n-mels)",
                                                     NULL);
  bind_function(env, "esi-core--mel-spectrogram", mel_spectrogram_fn);

  emacs_value start_background_recording_fn =
      env->make_function(env, 2, 2, Fstart_background_recording,
                         "Start background audio recording with given sample-rate and "
                         "buffer duration in seconds. Return a user pointer.\n\n"
                         "\(fn sample-rate buffer-duration)",
                         NULL);

  bind_function(env, "esi-core--start-background-recording",
                start_background_recording_fn);

  emacs_value read_background_recording_buffer_fn =
      env->make_function(env, 1, 1, Fread_background_recording_buffer,
                         "Return background audio recording buffer.\n\n"
                        "\(fn recording-pointer)", NULL);

  bind_function(env, "esi-core--read-background-recording-buffer",
                read_background_recording_buffer_fn);

  emacs_value stop_background_recording_fn =
      env->make_function(env, 1, 1, Fstop_background_recording,
                         "Stop background recording from pointer and return buffer.\n\n"
                        "\(fn recording-pointer)", NULL);

  bind_function(env, "esi-core--stop-background-recording",
                stop_background_recording_fn);

  provide(env, "esi-core");

  return 0;
}

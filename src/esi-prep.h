// Audio preprocessing module, replicating a lot of librosa's (Python) API.

#include <complex.h>
#include <fftw3.h>
#include <math.h>
#include <stdlib.h>

fftw_complex *stft(float *samples, size_t n_samples, size_t n_fft,
                   size_t hop_length, size_t *n_rows, size_t *n_cols) {
  // TODO: Windowing, padding etc.

  size_t n_fft_out = 1 + floor(n_fft / 2);
  size_t n_frames = floor((n_samples - n_fft) / hop_length + 1);
  fftw_complex *stft_matrix = fftw_malloc(n_fft_out * n_frames * sizeof(fftw_complex));

  double* frame = fftw_malloc(n_fft * sizeof(double));
  fftw_complex* output = fftw_malloc(n_fft_out * sizeof(fftw_complex));
  fftw_plan plan = fftw_plan_dft_r2c_1d(n_fft, frame, output, FFTW_ESTIMATE);

  for (int i = 0; i < n_frames; i++) {
    for (int j = 0; j < n_fft; j++) {
      frame[j] = samples[j + i * hop_length];
    }

    fftw_execute(plan);

    for (int j = 0; j < n_fft_out; j++) {
      stft_matrix[i + j * n_frames] = output[j];
    }
  }

  *n_rows = n_fft_out;
  *n_cols = n_frames;
  fftw_destroy_plan(plan);
  fftw_free(frame);
  fftw_free(output);
  return stft_matrix;
}

double* spectrogram(float* samples, size_t n_samples, size_t n_fft,
                    size_t hop_length, size_t power) {
  size_t n_rows, n_cols;
  fftw_complex* stft_matrix = stft(samples, n_samples, n_fft, hop_length, &n_rows, &n_cols);
  double* sg_matrix = malloc(n_rows * n_cols * sizeof(double));

  for (int i = 0; i < (n_rows * n_cols); i++) {
    sg_matrix[i] = pow(cabs(stft_matrix[i]), power);
  }
  fftw_free(stft_matrix);

  return sg_matrix;
}

// NOTE: For most of the neural network based models, we might just stop here
//       and won't do the log + dct to get MFCC
double* melspectrogram(float* samples, size_t n_samples, size_t n_fft,
                       size_t hop_length) {
  // NOTE: Default power is 2
  double* sg_matrix = spectrogram(samples, n_samples, n_fft, hop_length, 2);

  // TODO: Dot with mel filter
}

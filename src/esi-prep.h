// Audio preprocessing module, replicating a lot of librosa's (Python) API.

#include <complex.h>
#include <fftw3.h>
#include <math.h>

fftw_complex *stft(float *samples, size_t n_samples, size_t n_fft,
                    size_t hop_length, size_t *n_rows, size_t *n_cols) {
  // TODO
  *n_rows = 1 + floor(n_fft / 2);
  *n_cols = floor((n_samples - n_fft) / hop_length + 1);
  fftw_complex *stft_matrix = fftw_malloc((*n_rows) * (*n_cols) * sizeof(fftw_complex));
  return stft_matrix;
}

double* spectrogram(float* samples, size_t n_samples, size_t n_fft,
                    size_t hop_length, size_t power) {
  size_t n_rows, n_cols;
  fftw_complex* stft_matrix = stft(samples, n_samples, n_fft, hop_length, &n_rows, &n_cols);
  double* sg_matrix = calloc(n_rows * n_cols, sizeof(double));

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

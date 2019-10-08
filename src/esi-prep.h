// Audio preprocessing module, replicating a little of librosa's (Python) API.
#pragma once

#include <complex.h>
#include <fftw3.h>
#include <math.h>
#include <stdlib.h>
#include <stdint.h>

double* linspace(double start, double end, size_t n) {
  double *items = malloc(sizeof(double) * n);
  double diff = (end - start) / (n - 1);

  for (size_t i = 0; i < n; i++) {
    items[i] = start + (i * diff);
  }

  return items;
}

float* pad_zero(float* array, size_t size, size_t pad_size) {
  float* padded_array = calloc(pad_size + size + pad_size, sizeof(float));

  for (size_t i = 0; i < size; i++) {
    padded_array[pad_size + i] = array[i];
  }

  return padded_array;
}

// Similar effects as np.pad(array, pad_size, "reflect")
float* pad_reflect(float* array, size_t size, size_t pad_size) {
  float *padded_array = pad_zero(array, size, pad_size);

  // TODO: - avoid copying here
  //       - support more mirrors
  //       - use a single closed form index and loop
  if (pad_size > size) {
    printf("Pad size larger than array size, returning 0 padded array as we don't support multiple mirrors.");
    return padded_array;
  }

  // Right pad
  for (size_t i = 0; i < pad_size; i++) {
    padded_array[pad_size + size + i] = array[size - i - 2];
  }

  // Left pad
  for (size_t i = 0; i < pad_size; i++) {
    padded_array[pad_size - i - 1] = array[i + 1];
  }
  return padded_array;
}

double* boxcar_window(size_t window_size) {
  double* window = malloc(sizeof(double) * window_size);
  for (size_t i = 0; i < window_size; i++) {
    window[i] = 1.0;
  }

  return window;
}

double* hanning_window(size_t window_size) {
  double* window = calloc(window_size, sizeof(double));
  double* angles = linspace(-M_PI, M_PI, window_size + 1);

  // NOTE: Changing this value should cover hamming too
  float alpha = 0.5;
  float coeffs[] = { alpha, 1.0 - alpha };

  for (size_t k = 0; k < 2; k++) {
    for (size_t i = 0; i < window_size; i++) {
      window[i] += coeffs[k] * cos(k * angles[i]);
    }
  }

  free(angles);
  // We are keeping the last value allocated. Since the apparent size is
  // window_size, we don't worry about people peeking at the last cell.
  return window;
}

fftw_complex *stft(float *samples, size_t n_samples, size_t n_fft,
                   size_t hop_length, size_t *n_rows, size_t *n_cols) {
  size_t pad_size = floor(n_fft / 2);
  size_t n_fft_out = 1 + pad_size;

  // For centering the stft frame number indices
  float* padded_samples = pad_reflect(samples, n_samples, pad_size);
  size_t n_padded_samples = pad_size + n_samples + pad_size;

  size_t n_frames = floor((n_padded_samples - n_fft) / hop_length + 1);
  fftw_complex *stft_matrix = fftw_malloc(n_fft_out * n_frames * sizeof(fftw_complex));

  double* frame = fftw_malloc(n_fft * sizeof(double));
  fftw_complex* output = fftw_malloc(n_fft_out * sizeof(fftw_complex));
  fftw_plan plan = fftw_plan_dft_r2c_1d(n_fft, frame, output, FFTW_ESTIMATE);

  double* window = hanning_window(n_fft);

  for (size_t i = 0; i < n_frames; i++) {
    for (size_t j = 0; j < n_fft; j++) {
      frame[j] = padded_samples[j + i * hop_length] * window[j];
    }

    fftw_execute(plan);

    for (size_t j = 0; j < n_fft_out; j++) {
      stft_matrix[i + j * n_frames] = output[j];
    }
  }

  *n_rows = n_fft_out;
  *n_cols = n_frames;
  fftw_destroy_plan(plan);
  free(window);
  fftw_free(frame);
  fftw_free(output);
  free(padded_samples);
  return stft_matrix;
}

double *spectrogram(float *samples, size_t n_samples, size_t n_fft,
                    size_t hop_length, size_t power, size_t *n_rows,
                    size_t *n_cols) {
  fftw_complex* stft_matrix = stft(samples, n_samples, n_fft, hop_length, n_rows, n_cols);
  double* sg_matrix = malloc(*n_rows * *n_cols * sizeof(double));

  for (size_t i = 0; i < ((*n_rows) * (*n_cols)); i++) {
    sg_matrix[i] = pow(cabs(stft_matrix[i]), power);
  }
  fftw_free(stft_matrix);

  return sg_matrix;
}

// NOTE: For most of the neural network based models, we might just stop here
//       and won't do the log + dct to get MFCC
double* melspectrogram(float* samples, size_t n_samples, size_t n_fft,
                       size_t hop_length, size_t *n_rows, size_t *n_cols) {
  // NOTE: Default power is 2
  double* sg_matrix = spectrogram(samples, n_samples, n_fft, hop_length, 2, n_rows, n_cols);

  // TODO: Dot with mel filter
}

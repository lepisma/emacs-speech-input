// Audio preprocessing module, replicating a little of librosa's (Python) API.
#pragma once

#include <complex.h>
#include <fftw3.h>
#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <cblas.h>

double* linspace(double start, double end, size_t n) {
  double *items = malloc(sizeof(double) * n);
  double diff = (end - start) / (n - 1);

  for (size_t i = 0; i < n; i++) {
    items[i] = start + (i * diff);
  }

  return items;
}

double* pad_zero(double* array, size_t size, size_t pad_size) {
  double* padded_array = calloc(pad_size + size + pad_size, sizeof(double));

  for (size_t i = 0; i < size; i++) {
    padded_array[pad_size + i] = array[i];
  }

  return padded_array;
}

// Similar effects as np.pad(array, pad_size, "reflect")
double* pad_reflect(double* array, size_t size, size_t pad_size) {
  double *padded_array = pad_zero(array, size, pad_size);

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
  double alpha = 0.5;
  double coeffs[] = { alpha, 1.0 - alpha };

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

fftw_complex *rfft(double *samples, size_t n_samples, size_t *n_output) {
  *n_output = floor(n_samples / 2) + 1;

  fftw_complex *fft_output = fftw_malloc(sizeof(fftw_complex) * (*n_output));
  fftw_plan plan = fftw_plan_dft_r2c_1d(n_samples, samples, fft_output, FFTW_ESTIMATE);
  fftw_execute(plan);

  fftw_destroy_plan(plan);
  return fft_output;
}

fftw_complex *stft(double *samples, size_t n_samples, size_t n_fft,
                   size_t hop_length, size_t *n_rows, size_t *n_cols) {
  size_t pad_size = floor(n_fft / 2);
  size_t n_fft_out = 1 + pad_size;

  // For centering the stft frame number indices
  double* padded_samples = pad_reflect(samples, n_samples, pad_size);
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

double *spectrogram(double *samples, size_t n_samples, size_t n_fft,
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

double hz_to_mel(double f) {
  double min_log_hz = 1000.0;

  if (f < min_log_hz) {
    return 3.0 * f / 200.0;
  } else {
    return 3.0 * (min_log_hz / 200.0) + log(f / min_log_hz) / (log(6.4) / 27.0);
  }
}

double mel_to_hz(double mel) {
  double min_log_hz = 1000.0;
  double min_log_mel = 3.0 * min_log_hz / 200.0;

  if (mel < min_log_mel) {
    return 200 * mel / 3.0 ;
  } else {
    return min_log_hz * exp((log(6.4) / 27.0) * (mel - min_log_mel));
  }
}

// Return center frequencies for mel bands. Note that we use Slaney method for
// all of the work with mels which is the default in librosa.
double* mel_frequencies(size_t n_mels, double fmin, double fmax) {
  double min_mel = hz_to_mel(fmin);
  double max_mel = hz_to_mel(fmax);

  double *freqs = linspace(min_mel, max_mel, n_mels);

  for (size_t i = 0; i < n_mels; i++) {
    freqs[i] = mel_to_hz(freqs[i]);
  }

  return freqs;
}

double* fft_frequencies(size_t sr, size_t n_fft) {
  return linspace(0, (double)sr / 2, 1 + floor(n_fft / 2));
}

// Return a mel filterbank matrix. Most of the explicitly unmentioned parameters
// default to the values set in librosa.
double* mel_filter(size_t sr, size_t n_fft, size_t n_mels) {
  size_t n_rows = n_mels;
  size_t n_cols = 1 + floor(n_fft / 2);

  double fmin = 0.0;
  double fmax = (double)sr / 2;
  double *weights = calloc(n_rows * n_cols, sizeof(double));

  double *fft_fs = fft_frequencies(sr, n_fft);
  double *mel_fs = mel_frequencies(n_mels + 2, fmin, fmax);

  double norm_multiplier;
  for (size_t i = 0; i < n_rows; i++) {
    norm_multiplier = 2.0 / (mel_fs[i + 2] - mel_fs[i]);

    for (size_t j = 0; j < n_cols; j++) {
      if (fft_fs[j] >= mel_fs[i]) {
        if (fft_fs[j] <= mel_fs[i + 1]) {
          // Rise
          weights[j + (i * n_cols)] = norm_multiplier *
                                      (fft_fs[j] - mel_fs[i]) /
                                      (mel_fs[i + 1] - mel_fs[i]);
        } else if (fft_fs[j] <= mel_fs[i + 2]) {
          // Fall
          weights[j + (i * n_cols)] = norm_multiplier *
                                      (mel_fs[i + 2] - fft_fs[j]) /
                                      (mel_fs[i + 2] - mel_fs[i + 1]);
        }
      }
    }
  }

  free(fft_fs);
  free(mel_fs);
  return weights;
}

// NOTE: For most of the neural network based models, we might just stop here
//       and won't do the log + dct to get MFCC
double* melspectrogram(double* samples, size_t n_samples, size_t sr, size_t n_fft,
                       size_t hop_length, size_t n_mels, size_t *n_cols) {
  // NOTE: Default power is 2
  size_t n_rows;
  double* sg_matrix = spectrogram(samples, n_samples, n_fft, hop_length, 2, &n_rows, n_cols);
  double* filterbank = mel_filter(sr, n_fft, n_mels);

  double* msg_matrix = calloc(n_mels * (*n_cols), sizeof(double));
  cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
              n_mels, (*n_cols), n_rows,
              1.0, filterbank, n_rows, sg_matrix, (*n_cols),
              0.0, msg_matrix, (*n_cols));
  free(filterbank);
  free(sg_matrix);
  return msg_matrix;
}

#include <stdio.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include "esi-prep.h"

#define EPSILON 1e-07

void match_arrays(double* a, double* b, size_t size) {
  for (size_t i = 0; i < size; i++) {
    assert_float_equal(a[i], b[i], EPSILON);
  }
}

void match_complex_arrays(fftw_complex* a, fftw_complex* b, size_t size) {
  for (size_t i = 0; i < size; i++) {
    assert_float_equal(creal(a[i]), creal(b[i]), EPSILON);
    assert_float_equal(cimag(a[i]), cimag(b[i]), EPSILON);
  }
}

void test_window(void **state) {
  double window_1[] = { 0.0, 0.75, 0.75 };
  match_arrays(hanning_window(3), window_1, 3);

  double window_2[] = {
    0.,
    0.0954915,
    0.3454915,
    0.6545085,
    0.9045085,
    1.,
    0.9045085,
    0.6545085,
    0.3454915,
    0.0954915
  };
  match_arrays(hanning_window(10), window_2, 10);
}

void test_stft(void **state) {
  double samples[] = { 0.0, 0.5, 1.0 };

  size_t n_rows, n_cols;

  fftw_complex truth_1[] = {
    0.0 + 0.0 * I,
    0.5 + 0.0 * I,
    1.0 + 0.0 * I,
    0.5 + 0.0 * I,
    0.0 + 0.0 * I,
    -0.5 + 0.0 * I,
    -1.0 + 0.0 * I,
    -0.5 + 0.0 * I
  };
  fftw_complex* stft_matrix = stft(samples, 3, 2, 1, &n_rows, &n_cols);
  assert_int_equal(n_rows * n_cols, 8);
  match_complex_arrays(truth_1, stft_matrix, n_rows * n_cols);

  fftw_complex truth_2[] = {
    0.375 + 0.0 * I,
    1.125 + 0.0 * I,
    1.125 + 0.0 * I,
    -0.1875 + 0.3247595 * I,
    -0.5625 + 0.3247595 * I,
    -0.5625 - 0.3247595 * I
  };
  stft_matrix = stft(samples, 3, 3, 1, &n_rows, &n_cols);
  assert_int_equal(n_rows * n_cols, 6);
  match_complex_arrays(truth_2, stft_matrix, n_rows * n_cols);
}

void test_mel_frequencies(void **state) {
  double truth[] = {
    0.0, 213.11369888, 426.22739775, 639.34109663,
    852.45479551, 1069.957853, 1332.9552128, 1660.59774631,
    2068.77534111, 2577.2836447, 3210.78410654, 4000.0
  };
  double* mel_fs = mel_frequencies(12, 0, 4000);
  match_arrays(truth, mel_fs, 12);
}

void test_frequencies(void **state) {
  double truth[] = {
    0.0, 1378.125, 2756.25,
    4134.375, 5512.5, 6890.625,
    8268.75, 9646.875, 11025.0
  };
  double* fft_fs = fft_frequencies(22050, 16);
  match_arrays(truth, fft_fs, 9);
}

int main(void) {
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(test_stft),
    cmocka_unit_test(test_window),
    cmocka_unit_test(test_mel_frequencies),
    cmocka_unit_test(test_frequencies)
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}

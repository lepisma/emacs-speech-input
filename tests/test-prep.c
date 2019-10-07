#include <stdio.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include "esi-prep.h"

#define FLT_EPSILON 1e-07

void match_arrays(double* a, double* b, size_t size) {
  for (size_t i = 0; i < size; i++) {
    // TODO: FLT epsilon doesn't make sense here
    assert_float_equal(a[i], b[i], FLT_EPSILON);
  }
}

void match_complex_arrays(fftw_complex* a, fftw_complex* b, size_t size) {
  for (size_t i = 0; i < size; i++) {
    assert_float_equal(creal(a[i]), creal(b[i]), FLT_EPSILON);
    assert_float_equal(cimag(a[i]), cimag(b[i]), FLT_EPSILON);
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
  float samples[] = { 0.0, 0.5, 1.0 };

  size_t n_rows, n_cols;

  fftw_complex truth_1[] = { 0.5 + 0.0 * I, 1.50 + 0.0 * I, -0.5 + 0.0 * I, -0.5 + 0.0 * I };
  fftw_complex* stft_matrix = stft(samples, 3, 2, 1, &n_rows, &n_cols);
  assert_int_equal(n_rows * n_cols, 4);
  match_complex_arrays(truth_1, stft_matrix, n_rows * n_cols);

  fftw_complex truth_2[] = { 1.50 + 0.0 * I, -0.75 + 0.4330127 * I };
  stft_matrix = stft(samples, 3, 3, 1, &n_rows, &n_cols);
  assert_int_equal(n_rows * n_cols, 2);
  match_complex_arrays(truth_2, stft_matrix, n_rows * n_cols);
}

int main(void) {
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(test_stft),
    cmocka_unit_test(test_window)
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}

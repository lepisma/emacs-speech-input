#include <stdio.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdarg.h>
#include <stddef.h>
#include "esi-prep.h"

#define FLT_EPSILON 1e-07

void match_stft_arrays(fftw_complex* truth, fftw_complex* calculated, size_t size) {
  for (int i = 0; i < size; i++) {
    assert_float_equal(creal(calculated[i]), creal(truth[i]), FLT_EPSILON);
    assert_float_equal(cimag(calculated[i]), cimag(truth[i]), FLT_EPSILON);
  }
}

void test_stft(void **state) {
  float samples[] = { 0.0, 0.5, 1.0 };

  size_t n_rows, n_cols;

  fftw_complex truth_1[] = { 0.5 + 0.0 * I, 1.50 + 0.0 * I, -0.5 + 0.0 * I, -0.5 + 0.0 * I };
  fftw_complex* stft_matrix = stft(samples, 3, 2, 1, &n_rows, &n_cols);
  assert_int_equal(n_rows * n_cols, 4);
  match_stft_arrays(truth_1, stft_matrix, n_rows * n_cols);

  fftw_complex truth_2[] = { 1.50 + 0.0 * I, -0.75 + 0.4330127 * I };
  stft_matrix = stft(samples, 3, 3, 1, &n_rows, &n_cols);
  assert_int_equal(n_rows * n_cols, 2);
  match_stft_arrays(truth_2, stft_matrix, n_rows * n_cols);
}

int main(void) {
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(test_stft)
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}

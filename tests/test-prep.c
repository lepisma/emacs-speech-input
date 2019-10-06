#include <stdio.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdarg.h>
#include <stddef.h>
#include "esi-prep.h"

#define FLT_EPSILON 1e-07

void test_stft(void **state) {
  float samples[] = { 0.0, 0.5, 1.0 };
  fftw_complex truth[] = { 0.5 + 0.0 * I, 1.50 + 0.0 * I, -0.5 + 0.0 * I, -0.5 + 0.0 * I };

  size_t n_rows, n_cols;
  fftw_complex* stft_matrix = stft(samples, 3, 2, 1, &n_rows, &n_cols);

  for (int i = 0; i < n_rows * n_cols; i++) {
    assert_float_equal(creal(stft_matrix[i]), creal(truth[i]), FLT_EPSILON);
    assert_float_equal(cimag(stft_matrix[i]), cimag(truth[i]), FLT_EPSILON);
  }
}

int main(void) {
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(test_stft)
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}

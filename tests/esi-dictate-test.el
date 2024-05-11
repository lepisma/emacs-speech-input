;;; -*- lexical-binding: t; -*-

(require 'esi-dictate)

(ert-deftest test-esi-dictate-edits ()
  (ert-test-erts-file "esi-dictate.erts"
                      (lambda ()
                        (let ((splits (dictate-break-input (buffer-string))))
                          (delete-region (point-min) (point-max))
                          (insert (esi-dictate-make-edits (car splits) (cdr splits)))))))

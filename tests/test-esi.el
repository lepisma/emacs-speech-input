(require 'esi-core)

(describe "Version"
  (it "is correct"
    (expect (esi-core-version) :to-equal "0.0.1")))

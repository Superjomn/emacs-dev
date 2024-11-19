(require 'ert)
(load "~/emacs-dev/.doom.d/chun-core.el")

(ert-deftest chun-test ()
  (should (equal "chun-test" "chun-test")))

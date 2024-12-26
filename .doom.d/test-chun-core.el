(require 'ert)
(load "~/emacs-dev/.doom.d/chun-core.el")

(ert-deftest chun-test ()
  (should (equal "chun-test" "chun-test")))

(ert-deftest test-chun--str-split ()
  (should (equal (chun-str-split "a b c") '("a" "b" "c"))))

(ert-deftest test-chun--str-join ()
  (should (equal (chun-str--join " " '("a" "b" "c")) "a b c")))

(ert-deftest test-chun--str-join-1 ()
  (should (equal (chun-str--join "," '("a" "b" "c")) "a,b,c")))

(ert-deftest test-chun--str-trim ()
  (should (equal (chun-str--trim "  a b c  ") "a b c")))

(ert-run-tests-interactively t)

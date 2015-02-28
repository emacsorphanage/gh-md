(when (require 'undercover nil t)
  (undercover "gh-md.el"))

(require 'gh-md)

(ert-deftest impsort-basic-test ()
  (should (equal 1 1)))

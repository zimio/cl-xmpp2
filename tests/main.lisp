(defpackage cl-xmpp2/tests/main
  (:use :cl
        :cl-xmpp2
        :rove))
(in-package :cl-xmpp2/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-xmpp2)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

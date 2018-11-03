(defpackage openid-key-test
  (:use :cl
        :openid-key
        :1am))
(in-package :openid-key-test)

(defparameter *issuer* "https://accounts.google.com")

(test get-keys
  (let ((keys (get-openid-keys *issuer*)))
    (is keys)
    (let ((keys2 (get-openid-keys *issuer* :cached-keys keys)))
      (is (equal keys keys2)))))

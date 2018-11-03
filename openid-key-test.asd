#|
  This file is a part of openid-key project.
|#

;; (asdf:test-system :openid-key)

(defsystem "openid-key-test"
  :author "Koga Kazue"
  :license "MIT"
  :depends-on ("openid-key"
               "1am")
  :components ((:module "tests"
                :components
                ((:file "openid-key"))))
  :description "Test system for openid-key"

  :perform (test-op (op c) (symbol-call :1am :run)))

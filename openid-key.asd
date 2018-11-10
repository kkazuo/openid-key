#|
  This file is a part of openid-key project.
|#

(defsystem "openid-key"
  :version "0.1.1"
  :author "Koga Kazuo"
  :license "MIT"
  :depends-on ("cl-base64"
               "dexador"
               "ironclad"
               "jonathan"
               "local-time"
               "quri"
               "trivial-rfc-1123")
  :components ((:module "src"
                :components
                ((:file "openid-key"))))
  :description "Get OpenID keys from issuer."
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "openid-key-test"))))

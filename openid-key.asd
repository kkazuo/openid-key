#|
  This file is a part of openid-key project.
|#

(defsystem "openid-key"
  :version "0.1.0"
  :author "Koga Kazuo"
  :license "MIT"
  :depends-on ("dexador"
               "ironclad"
               "jonathan"
               "local-time"
               "qbase64"
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

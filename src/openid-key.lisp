(defpackage openid-key
  (:use :cl)
  (:export
   :openid-keys
   :get-openid-keys
   :find-openid-key))
(in-package :openid-key)

(defstruct openid-key
  kty
  use
  alg
  kid
  key)

(defstruct openid-keys
  issuer
  jwks-expires
  keys)

(defun issuer-endpoint (issuer)
  (let ((iss (quri:uri issuer)))
    (unless (quri:uri-domain iss)
      (setf iss (quri:make-uri :scheme "https" :host issuer)))
    iss))

(defun config-endpoint (endpoint)
  (let* ((well-known "/.well-known/openid-configuration")
         (config (quri:copy-uri endpoint))
         (path (quri.uri:uri-path config)))
    (if path
        (setf (quri:uri-path config) (concatenate 'string path well-known))
        (setf (quri:uri-path config) well-known))
    config))

(defun padding (s)
  (let ((p (- 4 (rem (length s) 4))))
    (case p
      ((1) (concatenate 'string s "="))
      ((2) (concatenate 'string s "=="))
      ((3) (concatenate 'string s "==="))
      (otherwise s))))

(defun b64-integer (s)
  (let* ((b (qbase64:decode-string (padding s) :scheme :uri))
         (v (make-array (length b) :element-type '(unsigned-byte 8) :initial-contents b)))
    (ironclad:octets-to-integer v)))

(defun jwk-key (jwk)
  (when (equal "RSA" (cdr (assoc "kty" jwk :test 'equal)))
    (let ((alg (cdr (assoc "alg" jwk :test 'equal)))
          (kid (cdr (assoc "kid" jwk :test 'equal)))
          (use (cdr (assoc "use" jwk :test 'equal)))
          (n (cdr (assoc "n" jwk :test 'equal)))
          (e (cdr (assoc "e" jwk :test 'equal))))
      (when (and (stringp alg)
                 (stringp kid)
                 (stringp use)
                 (stringp n)
                 (stringp e))
        (list
         (make-openid-key
          :kid kid
          :kty "RSA"
          :use use
          :alg alg
          :key (ironclad:make-public-key :RSA
                                         :n (b64-integer n)
                                         :e (b64-integer e))))))))

(defun get-expires (headers)
  (let ((expires (gethash "expires" headers)))
    (if expires
        (local-time:universal-to-timestamp (trivial-rfc-1123:parse-date expires))
        (local-time:timestamp+ (local-time:now) 12 :hour))))

(defun get-openid-keys (issuer &key cached-keys
                                 proxy
                                 (retry-times 2)
                                 (retry-interval 5))
  "Get JWKs from ISSUER, or nil

CACHED-KEYS --- an openid-keys for previous returned value, or nil.
PROXY --- a string for HTTP request proxy uri, or nil."
  (declare (type (or openid-keys null) cached-keys))
  (cond
    ((and (typep cached-keys 'openid-keys)
          (local-time:timestamp< (local-time:now)
                                 (openid-keys-jwks-expires cached-keys)))
     cached-keys)
    (t
     (handler-case
         (let* ((endpoint (issuer-endpoint issuer))
                (config (config-endpoint endpoint))
                (retry (dex:retry-request retry-times :interval retry-interval)))
           (multiple-value-bind (body)
               (handler-bind ((dex:http-request-failed retry))
                 (dex:get config :proxy proxy))
             (let ((jwks-uri (cdr (assoc "jwks_uri" (jojo:parse body :as :alist) :test 'equal)))
                   (retry (dex:retry-request retry-times :interval retry-interval)))
               (multiple-value-bind (body code headers)
                   (handler-bind ((dex:http-request-failed retry))
                     (dex:get jwks-uri :proxy proxy))
                 (declare (ignore code))
                 (let ((jwks-expires (get-expires headers))
                       (keys (cdr (assoc "keys" (jojo:parse body :as :alist) :test 'equal))))
                   (make-openid-keys
                    :issuer issuer
                    :jwks-expires jwks-expires
                    :keys (mapcan 'jwk-key keys)))))))
       (condition ())))))

(defun find-openid-key (kid keys)
  "Find key by key id from keys, or nil"
  (declare (type openid-keys keys))
  (let ((key (find-if (lambda (key)
                        (equal kid (openid-key-kid key)))
                      (openid-keys-keys keys))))
    (when key
      (values (openid-key-key key)
              (openid-key-alg key)
              (openid-key-kty key)
              (openid-key-use key)))))

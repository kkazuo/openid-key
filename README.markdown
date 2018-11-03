# Openid-Key

You can get public key of OpenID Connect id-token.
Then, you can verify id-token with the key.

## Usage

```
CL-USER> (setf *keys* (openid-key:get-openid-keys "https://accounts.google.com"))
#S(OPENID-KEY::OPENID-KEYS
   :ISSUER "https://accounts.google.com"
   :JWKS-EXPIRES @2018-11-03T20:03:46.000000+09:00
   :KEYS (#S(OPENID-KEY::OPENID-KEY
             :KTY "RSA"
             :ALG "RS256"
             :KID "8289d54280b76712de41cd2ef95972b123be9ac0"
             :KEY #<IRONCLAD::RSA-PUBLIC-KEY {1001EE03A3}>)
          #S(OPENID-KEY::OPENID-KEY
             :KTY "RSA"
             :ALG "RS256"
             :KID "aa436c3f63b281ce0d976da0b51a34860ff960eb"
             :KEY #<IRONCLAD::RSA-PUBLIC-KEY {1001F40433}>)))
CL-USER> (openid-key:find-openid-key "8289d54280b76712de41cd2ef95972b123be9ac0" *keys*)
#<IRONCLAD::RSA-PUBLIC-KEY {1001B512E3}>
"RS256"
"RSA"
```

You may also be interested in [Jose][Jose].

## Installation


[Jose]: http://quickdocs.org/jose/

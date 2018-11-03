# Openid-Key

You can get public key of OpenID Connect [id-token][id-token].
Then, you can verify id-token with the key.

OpedID Key format has defined in [JSON Web Key (JWK)][JWK].

## Usage

```
CL-USER> (setf *keys* (openid-key:get-openid-keys "https://accounts.google.com"))
#S(OPENID-KEY::OPENID-KEYS
   :ISSUER "https://accounts.google.com"
   :JWKS-EXPIRES @2018-11-03T20:03:46.000000+09:00
   :KEYS (#S(OPENID-KEY::OPENID-KEY
             :KTY "RSA"
             :USE "sig"
             :ALG "RS256"
             :KID "8289d54280b76712de41cd2ef95972b123be9ac0"
             :KEY #<IRONCLAD::RSA-PUBLIC-KEY {1001EE03A3}>)
          #S(OPENID-KEY::OPENID-KEY
             :KTY "RSA"
             :USE "sig"
             :ALG "RS256"
             :KID "aa436c3f63b281ce0d976da0b51a34860ff960eb"
             :KEY #<IRONCLAD::RSA-PUBLIC-KEY {1001F40433}>)))
CL-USER> (openid-key:find-openid-key "8289d54280b76712de41cd2ef95972b123be9ac0" *keys*)
#<IRONCLAD::RSA-PUBLIC-KEY {1001B512E3}>
"RS256"
"RSA"
"sig"
```

You may also be interested in [Jose][Jose] for verify [id-token][id-token].

## Installation


[Jose]: http://quickdocs.org/jose/
[JWK]: https://tools.ietf.org/html/rfc7517
[id-token]: https://openid.net/specs/openid-connect-core-1_0.html

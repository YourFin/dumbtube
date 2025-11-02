-- |

module Authn where

import Relude

{-
Works two ways:
1.
  Server ~somehow~ determines client identity
  Server sends client id forward, along with challenge
  Client decrypts client id into private key, signs challenge
  Server verifies challenge
2.
  Client visits page
  Page looks up authn stored in browser
  Client signs response


{
  rp: { // relying party
    id: <origin> i.e. https://5z.vc
    name: "5z.vc" // deprecated in new versions, but required
  },
  user: {
    id: <64 bytes>, // PK, random, sever side
    displayName: string, // user decides, at least 64 bytes
                         // Nickname profile enforcement?
    name: string, // i.e. email address
  },
  challenge: <bytes>, // random? crypto thing
  pubKeyCredParams: [
    { type: "public-key",
      alg: <long>, // IANA COSE Algorithms registry id for alg
    }
  ],
  timeout: <milliseconds>
  authenticatorSelection: {
    authenticatorAttachment?:
      "platform", // i.e. TPM, part of device
    | "cross-platform", // i.e. mobile phone with ble, ubikey
    // "touch the fingerprint reader"
    userVerification:
      "preferred" <default> //
    | "required"
    | "discouraged"
  } &
    // MUST be able to map origin -> identity on client
    ( { residentKey: "required", requireResidentKey: true }
    // Server may need to ship key forward via ID
    | { residentKey: "discouraged", reqireResidentKey: false (implicit) }
    | { residentKey: "preferred" }
    )
  },
  // public key
  excludeCredentials: [{ type: "public-key", id: <id> }],

}
-}

/*
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
    name: "5z.vc"
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
*/

/*
document.querySelector('#creds').addEventListener("click", async (e) => {
e.preventDefault();
let credsResults = await navigator.credentials.create({
  publicKey: {
    rp: { name: "localhost:3000", },
    user: {
      id: new Uint8Array([79, 252, 83, 72, 214, 7, 89, 26]),
      displayName: "Pen",
      name: "pen@localhost",
    },
    challenge: new Uint8Array([79, 252, 83, 72, 214, 7, 89, 26]),
    authenticatorSelection: {
      residentKey: "required",
      requireResidentKey: true,
      userVerification: "discouraged",
    },
    pubKeyCredParams: [
      { type: "public-key",
        alg: -7, // IANA COSE Algorithms registry id for alg
      },
      { type: "public-key",
        alg: -257,
      },
    ],
    hints: [ "client-device" ],
  },
});
let result = document.getElementById("results");
document.result = result;
result.textContent = JSON.stringify(result);
*/

export async function createCredentials(props: {
  generatedId: Uint8Array<ArrayBuffer>;
  challenge: Uint8Array<ArrayBuffer>;
  name: {
    display: string;
    id: string;
  };
  // In order of preference
  algorithms: number[];
  rpName: string;
}) {
  const result = await navigator.credentials.create({
    publicKey: {
      user: {
        id: props.generatedId,
        displayName: props.name.display,
        name: props.name.id,
      },
      authenticatorSelection: {
        residentKey: "required",
        requireResidentKey: true,
        userVerification: "discouraged",
      },
      challenge: props.challenge,
      pubKeyCredParams: props.algorithms.map((coseId) => ({
        type: "public-key",
        alg: coseId,
      })),
      rp: { name: props.rpName },
    },
  });
  if (result === null) {
    throw new Error("dumb");
  }
  result.type;
}

This is OCaml code for the book [Implementing SSL/TLS Using Cryptography and PKI](https://www.amazon.com/Implementing-SSL-TLS-Using-Cryptography/dp/0470920416/).

---

### WARNING
OCaml has **31 bit** integers, lest you get mindfucked (especially in hash functions).

---

### Ch 01 | Understanding Internet Security

- skipped proxy authorization in http client implementation
- there is some bizzare bug (at the end of the response, duplicate packets seem to be written by the `display result` function.
- for some reason "www.google.com" doesn't work, there has to be path along with a hostname. This seems to be an issue with how the GET request is being constructed (even the C implementation of the book suffers from this issue).

Since my primary objective is to get a high-level understanding of TLS works, I'm not addressing the above shortcomings.

---

### Ch 02 | Protecting Against Eavesdroppers

- skipped 3DES implementation
- skipped padding (leaving this up to the caller)
- RC4 has some other minor improvements which I've skipped

OCaml doesn't seem to be really suited to writing imperative code, I resorted to using Arrays and for loops extensively in the AES implementation and the code feels rather unnatural.

---

### Ch 03 | Secure Key Exchange over an Insecure Medium with Public Key Cryptography
- skipped padding checks for RSA implementation (Somewhat shockingly, my code worked on the first try!)
- Elliptic Curves for Diffie-Hellman don't seem to be implmented with huge numbers, so I don't think it's going to be used for the library. Perhaps it will come later.

Elliptic Curves are amazing. There are several properties of the cyclic group formed a curve that I'm not fully clear on -- but the math is not particularly complex, elementary group theory is all that is required. Also I'm using the Zarith library from opam (which itself uses GMP) instead of implementing my own arbitrary precision integer arithmetic library like the book does. I am somewhat baffled by the book's choice to do this though.

---

### Ch 04 | Authenticating Communications Using Digital Signatures
- sha256 initial hash given in the book is in little endian. The book's C implementation converts all the initial hashes to little endian format, but we don't need to this for the OCaml implemntation.
- finished generic digest routine to compute sha1, md5 and sha256 hashes of entire files.
- the ECDSA algorithm given in the book has a bug. It does not check if r and s are zeros (mod n). Strangely, it's not even there in the errata. The same bug inflicts the DSA algorithm implementation. Since this is rather unlikely, I haven't bothered to fix the bug in my code. I only noticed it after writing all the code.
- The implementation of add points and multiply point for elliptic curves is also wrong. It doesn't cover the cases where one point is at infinity, and doesn't handle the addition of a point and its inverse. I've fixed this in my code.

---

### Ch 05 | Creating a Network of Trust Using X.509 Certificates
- Comprehended the terrifying madness that is ASN.1. 

--- 

Read the protocol specification, and felt it was pointless coding the protocol as it is mostly glue code. Decided not to continue further. 

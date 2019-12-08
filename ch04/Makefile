ecdsa: ecdsa.ml ecc.ml dsa.ml digest.ml md5.ml sha.ml
	ocamlfind ocamlc -package hex -package integers -package zarith -linkpkg -g -o ecdsa md5.ml sha.ml digest.ml dsa.ml ecc.ml ecdsa.ml

dsa: dsa.ml digest.ml md5.ml sha.ml
	ocamlfind ocamlc -package hex -package integers -package zarith -linkpkg -g -o dsa md5.ml sha.ml digest.ml dsa.ml

hmac: hmac.ml digest.ml md5.ml sha.ml
	ocamlfind ocamlc -package hex -package integers -linkpkg -g -o hmac md5.ml sha.ml digest.ml hmac.ml

#digest: digest.ml md5.ml sha.ml
#	ocamlfind ocamlc -package hex -package integers -linkpkg -g -o digest md5.ml sha.ml digest.ml

clean:
	rm *.cm* ecdsa

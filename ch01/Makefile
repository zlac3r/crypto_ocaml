http: http.ml
	ocamlc -o http unix.cma http.ml

webserver: webserver.ml
	ocamlc -o webserver unix.cma webserver.ml

clean:
	rm *.cm* http webserver

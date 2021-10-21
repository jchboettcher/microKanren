MAIN = main

OBJS = ast.cmo printstream.cmo microkanren.cmo examples.cmo main.cmo

%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<

$(MAIN): clean $(OBJS)
	ocamlc -g -o $(MAIN) $(OBJS)
	rm -f *.cmo *.cmi

clean:
	rm -f $(MAIN)

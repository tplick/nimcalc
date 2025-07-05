default:
	ocamlopt.opt -c par.ml
	ocamlc -c par.ml
	ocamlopt.opt -c nimcalc.ml
	ocamlc -c nimcalc.ml
	ocamlopt.opt unix.cmxa par.cmx nimcalc.cmx queens.ml -o queens
	# ocamlopt.opt unix.cmxa par.cmx nimcalc.cmx queens_set.ml -o queens_set
	ocamlopt.opt unix.cmxa par.cmx nimcalc.cmx cram.ml -o cram
	ocamlopt.opt unix.cmxa par.cmx nimcalc.cmx beans.ml -o beans

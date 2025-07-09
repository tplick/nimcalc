default:
	ocamlopt.opt -I +unix -c par.ml
	ocamlc -I +unix -c par.ml
	ocamlopt.opt -I +unix -c nimcalc.ml
	ocamlc -I +unix -c nimcalc.ml
	ocamlopt.opt -I +unix unix.cmxa par.cmx nimcalc.cmx queens.ml -o queens
	# ocamlopt.opt -I +unix unix.cmxa par.cmx nimcalc.cmx queens_set.ml -o queens_set
	ocamlopt.opt -I +unix unix.cmxa par.cmx nimcalc.cmx cram.ml -o cram
	ocamlopt.opt -I +unix unix.cmxa par.cmx nimcalc.cmx beans.ml -o beans

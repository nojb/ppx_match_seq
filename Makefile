.PHONY: all
all:
	dune build

.PHONY: top
top:
	dune utop lib -- -require ppx_match_seq

.PHONY: clean
clean:
	dune clean

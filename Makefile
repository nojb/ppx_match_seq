.PHONY: all
all:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: top
top:
	dune utop lib -- -require ppx_match_seq # -dsource

.PHONY: clean
clean:
	dune clean

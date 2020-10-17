all:
	dune build
#	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install

.PHONY: all clean install test

# Developer rules

#coupling: .circleci/config.yml
#semgrep must be in your PATH (run 'pipenv shell' from semgrep/semgrep/)
check:
	@semgrep --version
	@semgrep-core -version
	semgrep --config ../semgrep.yml --config https://semgrep.dev/p/ocaml --error --strict --exclude tests ..

index:
	codegraph_build -lang cmt -derived_data .

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .

.PHONY: check index visual

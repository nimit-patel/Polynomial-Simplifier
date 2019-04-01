IN_FILES=$(wildcard tests/*.in)
OUT_FILES=$(IN_FILES:.in=.out)

default: main.byte

clean:
	rm -f main.byte
	rm -fr _build/
	rm -rf tests/*.out

main.byte:
	ocamlbuild -use-menhir -use-ocamlfind -pkg 'core,ppx_jane' main.byte -tag 'thread'

debug:
	ocamlbuild -use-menhir -use-ocamlfind -pkg 'core,ppx_jane' main.byte -tag 'thread,debug'

tests/%.out: tests/%.in main.byte
	./main.byte $< > $@

tests: main.byte $(OUT_FILES)
	@echo "Done testing. Results in tests dir."

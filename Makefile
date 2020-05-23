UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf64
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho64
endif
endif

main: $(shell find src -name "*.rs")
	cargo build
	mv target/debug/llisp main

test: compile.ml runner.ml test.ml parser.ml
	mkdir -p output
	$(BUILD) test.native
	mv test.native test

output/%.run: output/lib%.a runtime.rs
	rustc -l$(basename $(notdir $@)) -L./output -o $@ runtime.rs

output/lib%.a: output/%.o
	ar rcs $@ $<

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.llsp main
	mkdir -p output
	./main $< > $@

clean:
	rm -rf output/
	rm main

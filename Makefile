UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf64
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho64
endif
endif

.SECONDARY: output/test.ll

main: $(shell find src -name "*.rs")
	cargo build
	cp target/debug/llisp main

main.o: main.c
	clang -g -c $<

output/%.run: output/%.s main.o
	clang -g -mstackrealign -o $@ $< main.o

output/%.s: output/%.ll 
	llc -o $@ $<

output/%.ll: input/%.llsp main
	mkdir -p output
	./main $< > $@

clean:
	rm -rf output/
	rm main
	rm main.o

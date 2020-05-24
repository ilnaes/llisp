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

main.o: main.c
	clang -g -c $<

output/%.run: output/%.o main.o
	clang -g -mstackrealign -o $@ $< main.o

# output/%.rrun: output/lib%.a runtime.rs
# 	rustc -l$(basename $(notdir $@)) -L./output -o $@ runtime.rs

# output/lib%.a: output/%.o
# 	ar rcs $@ $<

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.llsp main
	mkdir -p output
	./main $< > $@

clean:
	rm -rf output/
	rm main
	rm main.o

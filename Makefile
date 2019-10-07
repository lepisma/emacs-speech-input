.PHONY: test

all: esi-core.so

test: tests/test-prep
	cask exec buttercup -L .
	tests/test-prep
	touch tests/test-prep.c

LIBS = -lsndfile -lfftw3 -lm

tests/test-prep: tests/test-prep.c esi-core.so
	gcc -lcmocka $(LIBS) -I ./src/ tests/test-prep.c -o $@

esi-core.so: src/esi-core.c src/esi-io.h src/esi-prep.h
	gcc $(LIBS) -fPIC -shared src/esi-core.c -o $@

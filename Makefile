.PHONY: test

all: esi-core.so

test: tests/test-prep esi-core.so
	cask exec buttercup -L .
	tests/test-prep
	rm tests/test-prep

LIBS = -lsndfile -lfftw3 -lm

tests/test-prep: tests/test-prep.c
	gcc -lcmocka $(LIBS) -I ./src/ tests/test-prep.c -o $@

esi-core.so: src/esi-core.c src/esi-io.h src/esi-prep.h
	gcc $(LIBS) -fPIC -shared src/esi-core.c -o $@

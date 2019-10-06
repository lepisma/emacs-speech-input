.PHONY: test

all: esi-core.so

test:
	cask exec buttercup -L .

LIBS = -lsndfile -lfftw3 -lm

esi-core.so: src/esi-core.c src/esi-io.h src/esi-prep.h
	gcc $(LIBS) -fPIC -shared src/esi-core.c -o $@

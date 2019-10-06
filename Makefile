.PHONY: test

all: esi-core.so

test:
	cask exec buttercup -L .

esi-core.so: src/esi-core.c src/esi-io.h
	gcc -lsndfile -fPIC -shared src/esi-core.c -o esi-core.so

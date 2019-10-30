.PHONY: test clean

all: esi-core.so

test: tests/test-prep
	cask exec buttercup -L .
	tests/test-prep
	touch tests/test-prep.c

LIBS = -lsndfile -lfftw3 -lm -lcblas -lsoundio

TORCH_DIR = ./resources/libtorch
TORCH_FLAGS = -I $(TORCH_DIR)/include -L $(TORCH_DIR)/lib -L ./lib \
	'-Wl,-rpath,$$ORIGIN/$(TORCH_DIR)/lib' '-Wl,-rpath,$$ORIGIN/lib' \
	-ltorch -lc10 -lpthread

clean:
	rm -f esi-core.so src/esi-embed.o

tests/test-prep: tests/test-prep.c esi-core.so
	gcc -lcmocka $(LIBS) -I ./src/ tests/test-prep.c -o $@

esi-core.so: $(wildcard src/*.c) $(wildcard src/*.h) src/esi-embed.o
	gcc $(LIBS) $(TORCH_FLAGS) -fPIC -pthread -shared $(wildcard src/*.c) src/esi-embed.o -o $@

src/esi-embed.o: src/esi-embed.cc
	g++ -std=c++17 $(TORCH_FLAGS) -fPIC -c src/esi-embed.cc -o $@

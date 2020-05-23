.PHONY: test clean

all: esi-core.so esi-embed-core.so

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
	rm -f esi-core.so esi-embed-core.so src/embed/esi-embed.o

tests/test-prep: tests/test-prep.c esi-core.so
	gcc -lcmocka $(LIBS) -I ./src/ tests/test-prep.c -o $@

esi-core.so: $(wildcard src/*.c) $(wildcard src/*.h)
	gcc $(LIBS) -fPIC -pthread -shared $(wildcard src/*.c) -o $@

esi-embed-core.so: src/embed/esi-embed-core.c src/embed/esi-embed.o
	gcc $(LIBS) $(TORCH_FLAGS) -I ./src/ -fPIC -pthread -shared src/embed/esi-embed.o src/embed/esi-embed-core.c -o $@

src/embed/esi-embed.o: src/embed/esi-embed.cc
	g++ -std=c++17 $(TORCH_FLAGS) -fPIC -c src/embed/esi-embed.cc -o $@

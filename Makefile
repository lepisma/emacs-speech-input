.PHONY: test

all: esi-core.so

test: tests/test-prep
	cask exec buttercup -L .
	tests/test-prep
	touch tests/test-prep.c

LIBS = -lsndfile -lfftw3 -lm -lcblas

TORCH_DIR = ./resources/libtorch
TORCH_FLAGS = -I $(TORCH_DIR)/include -L $(TORCH_DIR)/lib -L ./lib \
	'-Wl,-rpath,$$ORIGIN/$(TORCH_DIR)/lib' '-Wl,-rpath,$$ORIGIN/lib' \
	-ltorch -lc10 -lpthread

tests/test-prep: tests/test-prep.c esi-core.so
	gcc -lcmocka $(LIBS) -I ./src/ tests/test-prep.c -o $@

esi-core.so: src/esi-core.c src/esi-io.h src/esi-prep.h lib/libesitorch.so
	gcc $(LIBS) $(TORCH_FLAGS) -lesitorch -fPIC -shared src/esi-core.c -o $@

lib/libesitorch.so: src/esi-torch.cc
	g++ -std=c++17 $(TORCH_FLAGS) -fPIC -shared src/esi-torch.cc -o $@

pt-test: src/pt-test.cc
	g++ -std=c++17 $(TORCH_FLAGS) src/pt-test.cc -o $@

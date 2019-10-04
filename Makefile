esi-core.so: esi-core.o
	gcc -fPIC -shared -o $@ $<

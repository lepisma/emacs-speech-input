esi-core.so: src/esi-core.c
	gcc -fPIC -shared src/esi-core.c -o esi-core.so

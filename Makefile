
CFLAGS = -I$(HOME)/ext/emacs/src/  -I /usr/include/postgresql/
LDFLAGS =  -lpq
pq.so : pq.c
	gcc -g -O3 -shared $(CFLAGS) -Wall -Werror $(LDFLAGS)  $< -o $@

clean:
	rm -f pq.so

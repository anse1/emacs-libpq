EMACS = emacs

PG_CONFIG = pg_config
PGINCLUDEDIR := $(shell $(PG_CONFIG) --includedir)

CC = gcc
CFLAGS  = -I$(CURDIR) -I$(HOME)/ext/emacs/src/ -I$(PGINCLUDEDIR) -std=gnu99 -ggdb3 -Wall -fPIC
LDFLAGS = -lpq

ifeq ($(OS),Windows_NT)
TARGET = pq.dll
else
TARGET = pq.so
endif

all: $(TARGET)

%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	$(RM) $(TARGET)

check: $(TARGET)
	$(EMACS) --batch -Q -l ert -l test.el -f ert-run-tests-batch-and-exit

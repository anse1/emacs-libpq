EMACS = emacs
EMACS_VERSION := $(shell $(EMACS) -q --batch --eval "(princ emacs-version)")
EMACS_MAJOR_VERSION := $(shell $(EMACS) -q --batch --eval "(princ emacs-major-version)")
EMACS_INCLUDE_DIR := $(wildcard /usr/include/emacs-$(EMACS_MAJOR_VERSION)*)
EMACS_SRC_DIR := /usr/share/emacs/$(EMACS_VERSION)

PG_CONFIG = pg_config
PGINCLUDEDIR := $(shell $(PG_CONFIG) --includedir)

CC = gcc
CFLAGS  = -I$(CURDIR) -I$(EMACS_INCLUDE_DIR) -I$(EMACS_SRC_DIR) -I$(PGINCLUDEDIR) -std=gnu99 -ggdb3 -Wall -fPIC
LDFLAGS = -lpq

ifeq ($(OS),Windows_NT)
TARGET = pq-core.dll
else
TARGET = pq-core.so
endif

all: $(TARGET)

%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	$(RM) $(TARGET)

check: $(TARGET)
	$(EMACS) --batch -Q -l ert -l pq-test.el -f ert-run-tests-batch-and-exit

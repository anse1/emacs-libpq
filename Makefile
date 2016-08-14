CC = gcc
CFLAGS  = -I$(HOME)/ext/emacs/src/ -I /usr/include/postgresql/ -std=gnu99 -ggdb3 -Wall -fPIC
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

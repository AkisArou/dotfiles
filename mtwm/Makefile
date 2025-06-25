CC = gcc
CFLAGS = -Wall -O2 $(shell pkg-config --cflags xcb xcb-keysyms)
LDFLAGS = $(shell pkg-config --libs xcb xcb-keysyms)

all: mtwm

mtwm: mtwm.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

clean:
	rm -f mtwm

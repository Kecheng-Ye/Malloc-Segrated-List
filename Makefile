#
# Students' Makefile for the Malloc Lab
#
TEAM = Kecheng Ye
VERSION = 1
HANDINDIR = /afs/cs.cmu.edu/academic/class/15213-f01/malloclab/handin

CC = gcc
CFLAGS = -Wall -O2 -m32

OBJS = mdriver.o memlib.o fsecs.o fcyc.o clock.o ftimer.o
implicit = $(OBJS) basic_implicit_mm.o
adv_implicit = $(OBJS) adv_implicit_mm.o
explicit = $(OBJS) basic_explicit_mm.o


mdriver_implicit: $(implicit)
	$(CC) $(CFLAGS) -o mdriver $(implicit)

mdriver_adv_implicit: $(adv_implicit)
	$(CC) $(CFLAGS) -o mdriver $(adv_implicit)

mdriver_explicit: $(explicit)
	$(CC) $(CFLAGS) -o mdriver $(explicit)

mdriver.o: mdriver.c fsecs.h fcyc.h clock.h memlib.h config.h mm.h
memlib.o: memlib.c memlib.h
basic_implicit_mm.o: basic_implicit_mm.c mm.h memlib.h
adv_implicit_mm.o: adv_implicit_mm.c mm.h memlib.h
basic_explicit_mm.o: basic_explicit_mm.c mm.h memlib.h
fsecs.o: fsecs.c fsecs.h config.h
fcyc.o: fcyc.c fcyc.h
ftimer.o: ftimer.c ftimer.h config.h
clock.o: clock.c clock.h

clean:
	rm -f *~ *.o mdriver



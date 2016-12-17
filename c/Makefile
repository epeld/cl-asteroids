CC = gcc
INFILE = main.c
OUTFILE = main.o
FLAGS = -Wall

GL=/usr/local/Cellar/freeglut/3.0.0/include/

build: $(INFILE)
	$(CC) $(FLAGS) $(INFILE) -o $(OUTFILE) -lzmq -lczmq -I$(GL)

clean:
	rm -f *.o

rebuild: clean build

events.o : events.h events.c
	gcc -c events.c

main.o : events.h main.c
	gcc -c main.c -I$(GL)

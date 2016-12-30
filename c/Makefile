CC = gcc
INFILE = main.c glut.c events.c
OUTFILE = main.o
FLAGS = -Wall

GL=/usr/local/Cellar/freeglut/3.0.0/include/

build: $(INFILE)
	$(CC) $(FLAGS) $(INFILE) -o $(OUTFILE) -I$(GL) -lGL -lglu -lglut -L/opt/X11/lib/

clean:
	rm -f *.o

rebuild: clean build

events.o : events.h events.c
	gcc -c events.c

main.o : events.h main.c
	gcc -c main.c -I$(GL)

glut.o : events.h ui.h
	gcc -c glut.c -I$(GL)

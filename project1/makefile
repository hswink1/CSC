CC=/usr/bin/g++34
debug=-g

p1: p1.o term.o  
	$(CC) -o p1 p1.o term.o $(debug)

p1.o: p1.cpp term.h 
	$(CC) -c p1.cpp  $(debug)

term.o: term.cpp term.h
	$(CC) -c term.cpp $(debug)	

clean:
	\rm -f *.o p1

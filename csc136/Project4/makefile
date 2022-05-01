debug=-g

p4: WordRec.o LinkedList.o p4.o
	g++ -o p4 p4.o WordRec.o LinkedList.o $(debug)

testLL: testLL.o LinkedList.o
	g++ -o testLL testLL.o LinkedList.o $(debug)
	
p4.o: p4.cpp WordRec.h
	g++ -c p4.cpp  $(debug)

WordRec.o: WordRec.cpp WordRec.h
	g++ -c WordRec.cpp $(debug)

LinkedList.o: LinkedList.cpp LinkedList.h LinkedList.tpp WordRec.h
	cp LinkedList.cpp temp.cpp
	cat LinkedList.tpp >> temp.cpp
	g++ -c temp.cpp -o LinkedList.o $(debug)
# Comment out next line until debugging is done and ready to submit
# 	\rm -f temp.cpp

testLL.o: testLL.cpp LinkedList.h LinkedList.tpp
	g++ -c testLL.cpp $(debug)

clean:
	\rm -f *.o p4


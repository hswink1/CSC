DebugFlag=-g

debug: debug.o demo.o
	g++ -o debug demo.o debug.o $(DebugFlag)

debug.o: debug.cpp demo.h
	g++ -c debug.cpp $(DebugFlag)

demo.o: demo.cpp demo.h
	g++ -c demo.cpp $(DebugFlag)

clean:
	\rm -f *.o debug

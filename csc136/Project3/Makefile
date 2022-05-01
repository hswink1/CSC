DebugFlag=-g
cc=g++34

testArray: Array.o testArray.o
	$(cc) -o testArray Array.o testArray.o $(DebugFlag)

tst: Array.o tst.o
	$(cc) -o tst Array.o tst.o $(DebugFlag)

testArray.o: testArray.cpp Array.h
	$(cc) -c testArray.cpp $(DebugFlag)

tst.o: tst.cpp Array.h
	$(cc) -c tst.cpp $(DebugFlag)

Array.o: Array.cpp Array.h
	$(cc) -c Array.cpp $(DebugFlag)

clean:
	\rm -f *.o test


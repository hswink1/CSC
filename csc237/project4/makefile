cc=g++
debugFlag=-g

tree: BinarySearchTree.o Treetest.o Term.o
	$(cc) -o tree Treetest.o BinarySearchTree.o Term.o $(debugFlag)

Term.o: Term.cpp Term.h
	$(cc) -c Term.cpp $(debugFlag)

Treetest.o: Treetest.cpp  BinarySearchTree.h Term.h
	$(cc) -c Treetest.cpp $(debugFlag)

BinarySearchTree.o: BinarySearchTree.cpp BinarySearchTree.h
	$(cc) -c BinarySearchTree.cpp $(debugFlag)


clean:
	\rm -f *.o testdate

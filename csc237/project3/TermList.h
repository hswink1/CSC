// File: TermList.h
// Abstract base class for containers of Terms
// Known subclasses:
//      TermArray
// Coming soon: ParallelArrayTermList, TermVectorList, ...

#ifndef TERMLIST_H
#define TERMLIST_H

#include <fstream>
#include <string>

using namespace std;

/**
* \file TermList.h
* \class TermList
* \brief Grants inherited functions for all subclasses of TermList, allowing
* them to be set up to be filled by a file, evaluated, printed iteratively,
* printed recursively, and for a single subclass printed by a pointer.
**/

class TermList {
public:

  // Place the line of text into the data structure
  virtual void readIntoList(string filename)=0;

  // Print the data iteratively
  virtual void printIteratively()=0;

  //Print the data recursively
  virtual void printRecursively(int count=0)=0;

  // Print the data Iteratively with a pointer
  virtual void printPtr() {}   // not pure virtual; why?

  // Evaluate the Polynomial
  virtual double operator()(double x) const=0;

};

#endif

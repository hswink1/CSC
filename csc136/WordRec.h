// File: WordRec.h
// Author: Dr. Spiegel
// Last Revision; Sept. 15, 2018
// A WordRec is a container used to track a word and its multiplicity in data 

#ifndef WORDREC_H
#define WORDREC_H
#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>

using namespace std;

class WordRec {
  public:

  // Constructor
  WordRec(string word="",int times=1);

  //Sets the WordRec's data member word
  //Mutator
  //IMPORT
  void setWord(string token);

  //Returns WordRec's word data member
  //Inspector
  //EXPORT
  string getWord() const;
  
  //Returns WordRec's count data member
  //Inspector
  //EXPORT
  int getCount() const;

  //Operator ++ overload(Pre and Post): Increments count
  //Mutator
  //IMPORT/EXPORT
  WordRec &operator++();
  WordRec operator++(int);

  //Operator < overload: 
  //Returns whether a WordRec's word is alphanumerically less than another 
  //WordRec's word
  //Facilitator
  //IMPORT
  bool operator<(const WordRec &right) const;

  //Operator == overload: 
  // Returns whether a WordRec's word is equal to another WordRec's word
  //Facilitator
  //IMPORT
  bool operator==(const WordRec &right) const;
  
  //Operator == overload:
  //Returns whether a WordRec is equal to a string
  //Facilitator
  //IMPORT
  bool operator==(string &right);

  private:

  //Contains a word from a file
  string word;
  //Contains the multiplicity of a word from a file
  int count;

  //Sets the WordRec data member count, private to prevent errors with copying
  //Mutator
  //IMPORT
  void setCount(int value);
};

//Operator << overload: Prints a WordRec object
//Inspector
//EXPORT
ostream &operator<<(ostream &out, const WordRec &right);

//Operator >> overload: Inputs token from file into a WordRec object
//Mutator
//IMPORT
ifstream &operator>>(ifstream &inf, WordRec &right);

#endif

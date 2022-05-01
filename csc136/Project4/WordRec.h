/************************************************************/
/* Contributor: Henry Winkleman 							*/
/* Edit Date: December 3, 2018 								*/
/* Due Date: December 5, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #4											*/
/* Filename: WordRec.h 										*/
/* Purpose: WordRec holds a string and an int based on the	*/
/*	occurences it is inputted. Usage is to count every word	*/
/*	used in a chosen file									*/
/************************************************************/ 

#ifndef WORDREC_H
#define WORDREC_H
#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>

using namespace std;

class WordRec {
  public:

  //Default WordRec construction
  WordRec(string word="",int count=1);

  //Sets
  //Changes the set word.
  //Mutator - passed string/IMPORT
  void setWord(string words);
  //Changes the set count
  //Mutator - passed int/IMPORT
  void setCount(int counts);

  //Gets
  //Returns word in the WordRec
  //Inspector - returned string/EXPORT
  string getWord() const;
  //Returns count in the WordRec
  //Inspector - returned int/EXPORT
  int getCount() const;

  //Operator ++ overload(Pre and Post): Increments and returns data member count
  //Pre-increment
  //Mutator - Returned int/EXPORT
  WordRec &operator++();
  //Post-increment
  //Mutator - Returned int/EXPORT
  //		passed int/IMPORT
  WordRec operator++(int);

  //Operator -- overload (Pre and Post): Decrements and returns data member count
  //Pre-decrement
  //Mutator - Returned int/EXPORT
  WordRec &operator--();
  //Post-decrement
  //Mutator - Returned int/EXPORT
  //		passed int/IMPORT
  WordRec operator--(int);

  //Operator () overload: Returns a smaller string of a word
  //Facilitator - passed int/IMPORT
  //			returned string/EXPORT
  string operator()(int number) const;

  //>=< Operators: checks for alphanumerical order of two WordRecs
  //Operator < overload: Returns if the left is less
  //Facilitator - passed WordRec/IMPORT
  //			returned bool/EXPORT
  bool operator<(const WordRec &right) const;

  //Operator <= overload: Returns if the left is less than or equal to the right
  //Facilitator - passed WordRec/IMPORT
  //			returned bool/EXPORT
  bool operator<=(const WordRec &right) const;

  //Operator > overload: Returns if the left is greater
  //Facilitator - passed WordRec/IMPORT
  //			returned bool/EXPORT
  bool operator>(const WordRec &right) const;

  //Operator >= overload: Returns if the left is greater than or equal to the right
  //Facilitator - passed WordRec/IMPORT
  //			returned bool/EXPORT
  bool operator>=(const WordRec &right) const;

  //Operator == overload: Returns if the two are equal
  //Facilitator - passed WordRec/IMPORT
  //			returned bool/EXPORT
  bool operator==(const WordRec &right) const;

  //Operator != overload: Returns if the two are not equal
  //Facilitator - passed WordRec/IMPORT
  //			returned bool/EXPORT
  bool operator!=(const WordRec &right) const;

  
  private:

  //Holds a given word
  string word;
  //Holds the number of times that word has appeared
  int count;
};

//Operator << overload: Outputs the word and count
//Facilitator - passed ostream/IMPORT/EXPORT
//			passed WordRec/IMPORT
//			returned ostream/EXPORT
ostream &operator<<(ostream &out, const WordRec &right);

//Operator <<= overload: Outputs just the word
//Facilitator - passed ostream/IMPORT/EXPORT
//			passed WordRec/IMPORT
//			returned ostream/EXPORT
ostream &operator<<=(ostream &stream, const WordRec &right);

//Operator >> overload: Allows elements to be inputted from a stream
//Mutator - passed ifstream/IMPORT
//			passed WordRec/IMPORT/EXPORT
ifstream &operator>>(ifstream &inf, WordRec &right);

#endif

/************************************************************/
/* Contributor: Henry Winkleman 							*/
/* Edit Date: December 3, 2018 								*/
/* Due Date: December 5, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #4											*/
/* Filename: WordRec.cpp									*/
/* Purpose: WordRec holds a string and an int based on the	*/
/*	occurences it is inputted. Usage is to count every word	*/
/*	used in a chosen file									*/
/************************************************************/ 

#include <iostream>
#include <string>
#include <iomanip>
#include <fstream>
#include "WordRec.h"

using namespace std;

//Default WordRec construction
WordRec::WordRec(string word,int count)
{
  setWord(word);	//First sets the given word
  setCount(count);	//then sets the given count.
}

//Sets
//Changes the set word.
void WordRec::setWord(string theWord)
{  word=theWord;	//Inputs passed element as the word.
}

//Changes the set count
void WordRec::setCount(int theCount)
{  count=theCount;	//Inputs passed element as the count.
}

//Gets
//Returns word in the WordRec
string WordRec::getWord() const
{return word;}

//Returns count in the WordRec
int WordRec::getCount() const
{return count;}

//Operator ++ overload(Pre): Increments data member count
WordRec &WordRec::operator++()
{
  setCount(getCount()+1);	//Gets the word count, then increases it by one.
  return *this;				//Returns changed count.
}

//Operator ++ overload(Post): Increments data member count
WordRec WordRec::operator++(int)
{
  WordRec temp=*this;		//Holds the current count.
  setCount(getCount()+1);	//Increases count by one.
  return temp;				//Returns the old count.
}

//Operator -- overload (Pre): Decrements data member count
WordRec &WordRec::operator--()
{
  setCount(getCount()-1);	//Gets the word count, then decreases it by one.
  return *this;				//Returns changed count.
}

//Operator -- overload (Post): Decrements data member count
WordRec WordRec::operator--(int)
{
  WordRec temp=*this;		//Holds the current count.
  setCount(getCount()-1);	//Decreases count by one.
  return temp;				//Returns the old count.
}

//Operator () overload: Returns a smaller version of a word
string WordRec::operator()(int number) const
{
	string smallString=getWord();				//Holds the word in WordRec
	smallString=smallString.substr(0,number);	//Shortens it to the first n characters.
	return smallString;							//Returns shortened string.
}

//>=< Operators: checks for alphanumerical order of two WordRecs
//Operator < overload: Returns if the left is less
bool WordRec::operator<(const WordRec &right) const
{return (getWord()<right.getWord());}

//Operator <= overload: Returns if the left is less than or equal to the right
bool WordRec::operator<=(const WordRec &right) const
{return (getWord()<=right.getWord());}

//Operator > overload: Returns if the left is greater
bool WordRec::operator>(const WordRec &right) const
{return (getWord()>right.getWord());}

//Operator >= overload: Returns if the left is greater than or equal to the right
bool WordRec::operator>=(const WordRec &right) const
{return (getWord()>=right.getWord());}
  
//Operator == overload: Returns if the two are equal
bool WordRec::operator==(const WordRec &right) const
{return(getWord()==right.getWord());}

//Operator != overload: Returns if the two are not equal
bool WordRec::operator!=(const WordRec &right) const
{return(getWord()!=right.getWord());}

//Operator << overload: Outputs the word and count
ostream &operator<<(ostream &out, const WordRec &right)
{
  out<<setw(15)<<right.getWord()<<setw(15)<<right.getCount();
  return out;
}

//Operator <<= overload: Outputs just the word
ostream &operator<<=(ostream &stream, const WordRec &right)
{
  stream<<setw(15)<<right.getWord();
  return stream;
}

//Operator >> overload: Allows elements to be inputted from a stream
ifstream &operator>>(ifstream &inf, WordRec &right)
{
  string nextWord;	//Creates a string
  inf>>nextWord;	//String is filled from stream
  right.setWord(nextWord);	//Word is set to the string
  right.setCount(1);		//Count is set to 1 as per default
  return inf;	//Returns the stream
}

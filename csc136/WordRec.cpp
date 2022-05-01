/************************************************************/
/* Author: Henry Winkleman 									*/
/* Creation Date: October 2, 2018 							*/
/* Due Date: October 6, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #2											*/
/* Filename: WordRec.cpp 									*/
/* Purpose: Definitions for header file WordRec.h			*/
/* 															*/
/************************************************************/

#include <iostream>	//Cout/cin commands
#include <iomanip>	//Text commands
#include <fstream>	//Stream commands	
#include "WordRec.h"

using namespace std;

// Constructor
WordRec::WordRec(string word,int times){
	word="";
	count=1;
}

//Sets
//Setting word
void WordRec::setWord(string token){
	word=token;
}

//Gets
//Getting word
string WordRec::getWord() const{
	return word;
}
//Getting count
int WordRec::getCount() const{
	return count;
}

//Overloaded Operators
//Prefix changes count and returns value
WordRec &WordRec::operator++(){
	setCount(getCount()+1);
	return *this;
}

//Postfix
WordRec WordRec::operator++(int){
	WordRec temp=*this;		//Holds old value
	setCount(getCount()+1);	//Changes value
	return temp;			//Returns old value
}

//Compares two WordRecs
bool WordRec::operator<(const WordRec &right) const{
	return(getWord()<right.getWord());
}

//Evaluation of two WordRecs
bool WordRec::operator==(const WordRec &right) const{
	return(getWord()==right.getWord());
}

//Able to alter private count value
void WordRec::setCount(int value){
	count=value;
}

//Simple output with word followed by count
ostream &operator<<(ostream &out, const WordRec &right){
	out<<setw(30)<<left<<right.getWord()<<right.getCount();
	return out;
}

//Changes WordRec token with one from stream
ifstream &operator>>(ifstream &inf, WordRec &right)
{ string token;
   inf>>token;
   right.setWord(token);
   return(inf);
}

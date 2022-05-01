/************************************************************/
/* Author: Henry Winkleman 									*/
/* Creation Date: October 2, 2018 							*/
/* Due Date: October 6, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #2											*/
/* Filename: p2.cpp 										*/
/* Purpose: 	*/
/* 															*/
/************************************************************/
#include <iostream>	//Cout/cin commands
#include <iomanip>	//Text commands
#include <fstream>	//Stream commands	
#include <cstdlib>	//Used for EXIT_FAILURE to end program
using namespace std;

//Prototypes
int openFile(ifstream&);

int main(){
	ifstream inf;
	string holster;
	
	openFile(inf);
	while (!inf.eof()){
	inf>>holster;
	cout<<holster;
	}
	
return 0;
}
/*************************************************************************/ //RIPPED
/*																		*/
/* Function name: openFile										*/
/* Description: User chooses file with words for the program			*/
/* Parameters: streamFile: string holding the name of the file to be opened - input/output */
/* Return Value: int - 0 for success, 1 to shut off the program			*/
/* 																		*/
/*************************************************************************/
int openFile(ifstream& inf){
	string file="KnightsofYore.txt";
	inf.open(file.c_str());
	return 0;
}
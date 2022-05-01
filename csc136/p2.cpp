/************************************************************/
/* Author: Henry Winkleman 									*/
/* Creation Date: October 2, 2018 							*/
/* Due Date: October 6, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #2											*/
/* Filename: p2.cpp 										*/
/* Purpose: A program to sort the words of a file and output*/
/* 			chosen data.									*/
/************************************************************/
#include <iostream>		//Cout/cin commands
#include <iomanip>		//Text commands
#include <fstream>		//Stream commands	
#include <cstdlib>		//Used for EXIT_FAILURE to end program
#include "WordRec.h"	//Contains class WordRec
using namespace std;

//Prototypes
void intro(int);
int openFile(ifstream&);
void recieveData(ifstream&, WordRec[], int, int&);
void sorter(WordRec[], int, int&);
void fullCounter(ifstream&, WordRec[], int);
void printer(WordRec[], int, int);

int main(){
	//Declarations
	const int MAXWORDS = 100;	//Max array size
	WordRec holder[MAXWORDS];	//Holds words and count from file
	int numElts=0;				//Number of elements
	ifstream inf;				//Stream declaration
	
	intro(MAXWORDS);	//Informs user of intention
	
	//Filestream opening
	if(openFile(inf)==1)	//User inputs and opens file, 1 for exit command
		return EXIT_FAILURE;
	while(!inf.eof()){			//While file has text
		if(numElts<MAXWORDS){	//If the array is not full
			recieveData(inf, holder, MAXWORDS, numElts);//Fills array
			sorter(holder, MAXWORDS, numElts);			//Sorts array and removes duplicates
		}
		else{									//If array is full
			fullCounter(inf, holder, numElts);	//Reads remaining words to see if they are duplicates
		}
	}
	
	printer(holder, MAXWORDS, numElts);
	
	return 0;
}
/*************************************************************************/
/*																		*/
/* Function name: 	intro												*/
/* Description: 	Explanation of program intention					*/
/* Parameters: 		MAXWORDS-IMPORT										*/
/* Return Value:	None												*/
/* 																		*/
/*************************************************************************/
void intro(int MAXWORDS){
	cout<<endl<<"*******************************************************"<<endl;
	cout<<"Choose a file to open and this program will sort every"<<endl;
	cout<<"word up to "<<MAXWORDS<<" words and count their occurences along"<<endl;
	cout<<"with a few options on what data you would like this"<<endl;
	cout<<"program to present."<<endl<<endl<<"Have fun!"<<endl;
	cout<<"*******************************************************"<<endl<<endl;
}

/*************************************************************************/
/*																		*/
/* Function name:	openFile											*/
/* Description: User chooses file with words for the program			*/
/* Parameters: 	ifstream inf - IMPORT/EXPORT, used to open filestream	*/
/* Return Value:	None												*/
/* 																		*/
/*************************************************************************/
int openFile(ifstream& inf){
	string file="";	//Safety for file input
	cout<<"Please choose a file to open (EXIT TO CANCEL): ";
	getline(cin, file);
	if(file=="EXIT"||file=="exit"){	//Checks for exit command
		cout<<endl<<"Exiting."<<endl<<endl;
		return 1;
	}
	inf.open(file.c_str());			//Opens the stream
	while(inf.fail()){				//Failure allows user to try again
		cout<<endl<<"File unaccessable, try again (EXIT TO CANCEL): ";
		getline(cin, file);
		if(file=="EXIT"||file=="exit"){	//Checks for exit command again
			cout<<endl<<"Exiting."<<endl<<endl;
			return 1;
		}
		inf.open(file.c_str());			//Opens the stream, failure resets loop
	}
}

/*************************************************************************/
/*																		*/
/* Function name:	recieveData											*/
/* Description:		Fills array with strings from the file				*/
/* Parameters:	inf-IMPORT, holder[]-EXPORT, MAXWORDS-IMPORT,			*/
/*				numElts-IMPORT/EXPORT									*/
/* Return Value:	None												*/
/* 																		*/
/*************************************************************************/
void recieveData(ifstream& inf, WordRec holder[], int MAXWORDS, int& numElts){
	while(!inf.eof()&&numElts!=MAXWORDS){	//Stops at end of file or array filled
		inf>>holder[numElts];				//String from file enters WordRec
		numElts++;							//Elements increased
	}
	if(inf.eof()){						//If ended from end of file
		holder[numElts].setWord("");	//Last index is set to an empty string
		numElts--;						/*Removes last element because it will be
										the null value at end of file*/
	}
}

/*************************************************************************/
/*																		*/
/* Function name: 	sorter												*/
/* Description: 	Sorts elements in WordRec							*/
/* Parameters: 		holder[]-IMPORT/EXPORT, numElts-IMPORT/EXPORT		*/
/* 					MAXWORDS-IMPORT										*/
/* Return Value:	None												*/
/* 																		*/
/*************************************************************************/
void sorter(WordRec holder[], int MAXWORDS, int& numElts){
	for(int spot=0;spot<=numElts;spot++){	//Sorts for every index
		int idxMin=spot;					//Lowest word is assumed to be that in spot
		//Test for lowest value starts at the right of spot until end of elements
		for(int idx=spot+1;idx<numElts;idx++){
			if(holder[idx]<holder[idxMin])	//If the select index is less than the min, the min is set to it
				idxMin=idx;
			else if(holder[idx]==holder[spot]){	//If found to be a duplicate
				holder[spot]++;					//Count is increase on original
				for(int tempIdx=idx;tempIdx<numElts-1;tempIdx++){	//Moves all words from index to the left
					holder[tempIdx]=holder[tempIdx+1];				//Overwrites values to the left
				}
				numElts--;	//Lowers numElts to account for removed word
				idx--;		//Resets cycle for that index
			}
		}
		if(idxMin>spot){				//If min is not the same as spot
			WordRec temp[1];			//Temporary spot to exchange values
			temp[0]=holder[spot];		//Copies holder[spot] values into a temp variable
			holder[spot]=holder[idxMin];//Minimum takes the spot index
			holder[idxMin]=temp[0];		//Puts spot values where minimum index was
			spot--;						//Resets cycle for the spot to catch duplicates
		}
	}
}

/*************************************************************************/
/*																		*/
/* Function name: 	fullCounter											*/
/* Description: If array is full, 										*/
/*				checks if remaining words are duplicates				*/
/* Parameters:	inf-IMPORT, holder[]-IMPORT/EXPORT,			 			*/
/*				numElts-IMPORT											*/
/* Return Value:	None												*/
/* 																		*/
/*************************************************************************/
void fullCounter(ifstream& inf, WordRec holder[], int numElts){
	string tempString;		//Temporary string
	while(!inf.eof()){		//Until end of file
		inf>>tempString;	//String in file is held in temp space
		for(int idx=0;idx<=numElts;idx++){	//Checks all indexes
			/*If temp is the same as something in WordRec, increases
			counter in WordRec*/
			if(tempString==holder[idx].getWord())
				holder[idx]++;
		}
	}
}

/*************************************************************************/
/*																		*/
/* Function name: printer												*/
/* Description: Final output for program								*/
/* Parameters: holder[], numElts BOTH EXPORT							*/
/* Return Value:	None												*/
/* 																		*/
/*************************************************************************/
void printer(WordRec holder[], int MAXWORDS, int numElts){
	if(numElts==0)	//If no elements found, ends program
		cout<<"No possible output, ending program."<<endl<<endl;
	else{
		cout<<endl<<"*******************************************************"<<endl;
		cout<<"You can output the data in one of four ways:"<<endl;
		cout<<"A. Every word and their occurences up to "<<MAXWORDS<<" words."<<endl;
		cout<<"P. Words that occurred a specific number of times."<<endl;
		cout<<"S. Only a specified amount of letters of every word."<<endl;
		cout<<"F. Search for a specific word."<<endl;
		cout<<"Q. Exit the program."<<endl;
		cout<<"*******************************************************"<<endl<<endl;
		cout<<"Choice of output: ";
		string choice;
		getline(cin, choice);	//User enters choice of output
		cout<<endl;
		//If they enter an incorrect option
		while(choice!="A"&&choice!="a"&&choice!="P"&&choice!="p"&&choice!="S"&&choice!="s"&&choice!="F"&&choice!="f"&&choice!="Q"&&choice!="q"){
			cout<<"Invalid choice, try again: ";
			getline(cin, choice);	//User enters choice of ouput again
		}
		//All words
		if(choice=="A"||choice=="a"){
			cout<<"*******************************************************"<<endl;
			cout<<"All words chosen."<<endl<<endl;
			cout<<setw(30)<<left<<"Word:"<<"Count:"<<endl<<endl;
			for(int idx=0;idx<numElts;idx++){	//Uses default output in header for every index
				cout<<holder[idx]<<endl;
			}
		}
		//Count
		else if(choice=="P"||choice=="p"){
			cout<<"Enter an amount of occurences: ";
			int tempCount=0;
			cin>>tempCount;	//User inputs desired count
			cout<<endl;
			while(tempCount<1){	//Check for valid count
				cout<<"Invalid number, try again: ";
				cin.clear();	//Clears input stream in case of character entry
				cin.ignore(1000, '\n');
				tempCount=0;
				cin>>tempCount;	//User inputs desired count
				cout<<endl;
			}
			cout<<"*******************************************************"<<endl;
			cout<<"Words that appeared "<<tempCount<<" times."<<endl<<endl;
			cout<<setw(30)<<left<<"Word:"<<"Count:"<<endl<<endl;
			for(int idx=0;idx<numElts;idx++){	//Uses default output in header for every index
				if(holder[idx].getCount()==tempCount)	//Displays all words with select count
					cout<<holder[idx]<<endl;
			}
		}
		//Letters
		else if(choice=="S"||choice=="s"){
			cout<<"How many characters are to be shown: ";
			int tempChars=0;
			cin>>tempChars;	//User inputs desired amount of characters
			cout<<endl;
			while(tempChars<1){	//Check for valid count
				cout<<"Invalid amount, try again: ";
				cin.clear();	//Clears input stream in case of character entry
				cin.ignore(1000, '\n');
				tempChars=0;
				cin>>tempChars;	//User inputs desired amount of characters
				cout<<endl;
			}
			cout<<"*******************************************************"<<endl;
			cout<<"Displaying "<<tempChars<<" characters."<<endl<<endl;
			cout<<setw(30)<<left<<"Word:"<<"Count:"<<endl<<endl;
			for(int idx=0;idx<numElts;idx++){	//Uses default output in header for every index
				string reducedString=holder[idx].getWord();	//Pulls string from WordRec
				reducedString=reducedString.substr(0, tempChars);	//Reduces size of string
				//Displays reduced size and count
				cout<<setw(30)<<left<<reducedString<<holder[idx].getCount()<<endl;
			}
		}
		//Find
		else if(choice=="F"||choice=="f"){
			cout<<"What word is to be found: ";
			string tempWord="";
			getline(cin, tempWord);	//User enters word to be found
			cout<<endl;
			while(tempWord==""){	//Check for blank
				cout<<"No word entered, try again: ";
				getline(cin, tempWord);	//User enters word to be found
				cout<<endl;
			}
			cout<<"*******************************************************"<<endl;
			cout<<"Finding "<<tempWord<<"."<<endl<<endl;
			cout<<setw(30)<<left<<"Word:"<<"Count:"<<endl<<endl;
			int notfound=0;
			for(int idx=0;idx<numElts;idx++){		//Uses default output in header for every index
				if(holder[idx].getWord()==tempWord)	//Displays found word
					cout<<holder[idx]<<endl;
				else
					notfound++;	//Signifies word wasn't found
			}
			if(notfound==numElts){	//If word wasn't found
				cout<<endl<<"Word not found."<<endl;
			}
		}
		//Quit
		else
			cout<<"Quit chosen."<<endl<<endl;
	}
	cout<<endl<<endl<<"Ending program. Have a nice day!"<<endl;
	cout<<"*******************************************************"<<endl<<endl;
}

/************************************************************/
/* Author: Henry Winkleman 									*/
/* Creation Date: September 3, 2018 						*/
/* Due Date: September 8, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #1											*/
/* Filename: p1.cpp 										*/
/* Purpose: This program finds the word in a text document	*/
/* 			and counts how many times they appear			*/
/* 															*/
/************************************************************/
#include <iostream>	//Cout/cin commands
#include <iomanip>	//Text commands
#include <fstream>	//Stream commands	
#include <cstdlib>	//Used for EXIT_FAILURE to end program
using namespace std;

//Structure Construction
struct uniqueToken{
		string token;
		int count;} library[100];
		
//Prototypes
void start();
int declareStreamFile(string&);
int checkUnique(int&, string, uniqueToken[]);
void checkSort(int, string, uniqueToken[]);
int checkCount(int, string, uniqueToken[]);
void finalOutput(int, uniqueToken[]);

int main(){
	
	int numElts=0;					//Number of elements in use for the array
	string streamFile, tempToken;
	ifstream ins;					//Start filestream
	

	//Program introduction and stream opening
	start();								//Presents program goal
	if (declareStreamFile(streamFile)==1){	//Asks user for file name
		return EXIT_FAILURE;				//Exit command to end program at the beginning
	}	
	ins.open(streamFile.c_str());							//Opens stream
	ins>>library[numElts].token;							//Starting token
	library[numElts].count=1;
	ins>>tempToken;											//Moves string from file to a temporary space
	while (!ins.eof()){										//Checks for items left in the file
		if(checkUnique(numElts, tempToken, library)==0){	//Checks if the token or string is unique in the library
			checkSort(numElts, tempToken, library);}
		checkCount(numElts, tempToken, library);			//Adds 1 to the count for the token
		ins>>tempToken;										//Moves string from file to a temporary space
	}
	finalOutput(numElts, library);							//Displays all tokens and counts
return 0;}
/*************************************************************************/
/* 																		*/
/* Function name: start													*/
/* Description: Shows program's function and goal						*/
/* Parameters: None														*/
/* Return Value: None													*/
/* 																		*/
/*************************************************************************/
void start(){
	cout<<endl<<"--------------------------------------------------"<<endl;
	cout<<"This program will take any ordinary text file,"<<endl;
	cout<<"find every instance of a different word and"<<endl;
	cout<<"count the amount of times it appears. The total"<<endl;
	cout<<"will then be displayed."<<endl;
	cout<<"--------------------------------------------------"<<endl<<endl;
}
/*************************************************************************/
/*																		*/
/* Function name: declareStreamFile										*/
/* Description: User chooses file with words for the program			*/
/* Parameters: streamFile: string holding the name of the file to be opened - input/output */
/* Return Value: int - 0 for success, 1 to shut off the program			*/
/* 																		*/
/*************************************************************************/
int declareStreamFile(string& streamFile){
	cout<<"Enter the name of the file you wish to open ('N' to stop): ";
	getline(cin, streamFile);										//Gets file name
	cout<<endl;
	ifstream ins;													//Declares stream
	if (streamFile=="N")											//"N" is used to end the program
				return 1;											//Returns exit command
	ins.open(streamFile.c_str());									//Tests opening the file
		while(ins.fail()){											//Failure alows user to try a different file, loops
			cout<<"File not found, please enter another ('N' to stop): ";
			getline(cin, streamFile);
			cout<<endl;
			if (streamFile=="N")
				return 1;
			ins.open(streamFile.c_str());
			}
return 0;}
/*************************************************************************/
/* 																		*/
/* Function name: checkUnique											 */
/* Description: Checks index in structure for identical tokens			*/
/* Parameters: numElts - number of indexes in the structure - input/output	*/
/* tempToken: word pulled from filestream to be checked - input 		*/
/* library - structure holding the tokens and counts - input			*/
/* Return Value: int - 0 for new token, 1 for identical token			 */
/* 																		*/
/*************************************************************************/
int checkUnique(int& numElts, string tempToken, uniqueToken library[]){
	for(int i=0;i<=numElts;i++){			//Runs for first array entry until the last element used
		if (tempToken==library[i].token){
			return 1;}}						//If found to be not unique, func ends
	numElts++;								//If unique, new space is made in the array
return 0;}	
/*************************************************************************/
/*																		 */
/* Function name: checkSort												 */
/* Description: Arranges all tokens alphabetically, and creates a space to drop the new token */
/* Parameters: numElts - number of indexes in the structure - input		*/
/* tempToken: word pulled from filestream to be checked - input 		*/
/* library - structure holding the tokens and counts - input*/
/* Return Value: None													*/
/* 																		*/
/*************************************************************************/
void checkSort(int numElts, string tempToken, uniqueToken library[]){
	int i=numElts;
	if(tempToken<library[numElts-1].token){					//If new unique token is before (alphabetically) the token in the previous slot
		for(;i>0&&tempToken<library[i-1].token;i--){		//Checks how many tokens the placeholder should be before
				library[i].token=library[i-1].token;		//Moves tokens in array to the next place as it keeps finding more
				library[i].count=library[i-1].count;
				if(i-1==0){									//If at the end of the array, the held token becomes the first of the array
					library[0].token=tempToken;
					library[0].count=0;}}		
		if(i!=0&&tempToken>library[i-1].token){
			library[i].token=tempToken;						//Drops the held token in the empty space
			library[i].count=0;}}							//Fresh counter
	else{													//If new token is the greatest, dropped in new spot
		library[numElts].token=tempToken;
		library[numElts].count=0;}
}
/*************************************************************************/
/*																		*/
/* Function name: checkCount 											*/
/* Description: Adds 1 to the count of the matching word in tempToken	*/
/* Parameters: numElts - number of indexes in the structure - input		*/
/* tempToken: word pulled from filestream to be checked - input 		*/
/* library - structure holding the tokens and counts - input			*/
/* Return Value: None													*/
/* 																		*/
/*************************************************************************/
int checkCount(int numElts, string tempToken, uniqueToken library[]){
	for(int i=0;i<=numElts;i++){					//Runs through the whole array
		if (tempToken==library[i].token){			//Checks for equivalent value
			library[i].count=library[i].count+1;	//Adds one to the count of identical token
			return 0;								//Ends the for loop after the first success
}}}
/*************************************************************************/
/* 																		*/
/* Function name:	finalOutput 										*/
/* Description:		Displays count and name of tokens in the library	*/
/* Parameters:		numElts - number of indexes in the structure - input*/
/* library - structure holding the tokens and counts - input			*/
/* Return Value: 	None												*/
/* 																		*/
/*************************************************************************/
void finalOutput(int numElts, uniqueToken library[]){
	cout<<"----------------------------------------"<<endl;
	cout<<setw(30)<<left<<"Unique Token:"<<"Count:"<<endl<<endl;
	for(int i=0;i<=numElts;i++){
		cout<<setw(30)<<left<<library[i].token<<library[i].count<<endl;	//Lists every token and count next to it	
	}
	cout<<endl<<"Program complete."<<endl;
	cout<<"----------------------------------------"<<endl<<endl;
}
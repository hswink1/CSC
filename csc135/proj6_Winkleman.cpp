/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 4/30/18
     Due Date:      5/2/18   
     Course:        CSC135-020 
     Professor:     Dr. Joo Tan 
     Assignment:    Project 6
     Filename:      proj6_Winkleman.cpp
     Purpose:       A program used to help Santa determine the
					efficiency of each elf.

******************************************************************/

//Libraries
#include <iostream>	//Basic library for input/output
#include <iomanip>	//Text formatting
#include <cstdlib>	//Used for "cerr" command
#include <fstream>	//File stream
#include <sstream> 	//String stream
using namespace std;

//Prototypes
void intro();
string inputFile();
int error();
void assignRating(int[], string[], int, int&);
int totalToys(int[], int);
string findHigh(string[], int[], int);
string findLow(string[], int[], int);
void displayAnalysis(string[], int[], string[], int, int, int, string, string);

int main(){
	
	//Local Variables
	string inFile, worker[50], rating[50], value;
	int toys[50];
	int counter=0;
	int topWorker=0;
	
	//Streams
	ifstream ins;	//Input stream creation
	
	//Start Program
	intro();
	inFile=inputFile();
	ins.open(inFile.c_str());
	
	//Error Check
	while (ins.fail())
	{
		int restart=error();
		if (restart==0){
			inFile=inputFile();
			ins.open(inFile.c_str());
		}
		else{
			return EXIT_FAILURE;
		}
	}
	
	//Pull Stream Data
	while (getline(ins, value)){			//Pulls lines from stream
		istringstream stringdata(value);	//Convert to input string
		while(stringdata>>worker[counter]){						//If names are available an array space will be taken
			stringdata>>toys[counter];							//Second string is the toys they made
			assignRating(toys, rating, counter, topWorker);		//Rating based on toys
			counter++;											//Keeps track of how high the array gets
		}
	}
	ins.close();										//Input stream closes
	int toysMade=totalToys(toys, counter);				//Finds total toys made
	string high=findHigh(worker, toys, counter);		//Finds best worker
	string low=findLow(worker, toys, counter);			//Finds works worker
	displayAnalysis(worker, toys, rating, counter, topWorker, toysMade, high, low);

return 0;}

void intro(){
	cout<<endl;
	cout<<"--------------------------------------------------"<<endl;
	cout<<"Hello, this program will read from a chosen file"<<endl;
	cout<<"and determine who is hard working or not based"<<endl<<endl;
	cout<<"on their performance in making toys."<<endl;
	cout<<"--------------------------------------------------"<<endl<<endl;
}
/*************************************************************************

    Function name: 	intro
    Description: 	Prints the program intent.
    Parameters: 	None
    Return Value: 	None
 
*************************************************************************/

string inputFile(){
	string tempInFile;
	cout<<"Please enter the name of the file to be read: ";
	getline(cin, tempInFile);
	return tempInFile;
}
/*************************************************************************

    Function name:	inputFile
    Description:	User chooses input file.
    Parameters:		None
    Return Value:	tempInFile - string name for file
 
*************************************************************************/

int error(){
	string errResponse="temp";
	cout<<"File cannot be opened, try another file (Y/N): ";
	while ((errResponse!="Y")&&(errResponse!="N")){
		getline(cin, errResponse);
		if (errResponse=="Y")
			return 0;
		else if (errResponse=="N")
			return 1;
		else
			cout<<"Please enter a valid response: ";
	}
}
/*************************************************************************

    Function name:	error
    Description:	Prompts the user to restart or not.
    Parameters:		None
    Return Value:	0 or 1, signals restart or not.
 
*************************************************************************/

void assignRating(int toys[], string rating[], int counter, int& topWorker){
	if (toys[counter]<200){
		rating[counter]="-";
	}
	else if (toys[counter]<300&&toys[counter]>=200){
		rating[counter]="*";
	}
	else if (toys[counter]<500&&toys[counter]>=300){
		rating[counter]="***";
	}
	else{
		rating[counter]="*****";
		topWorker++;
	}
}
/*************************************************************************

    Function name:	assignRating
    Description:	Gives rating based on performance
    Parameters:		rating - rating which correlates with toys made
					toys - toys worker made
					counter - number for parallel array
    Return Value:	None
 
*************************************************************************/

int totalToys(int toys[], int counter){
	int total=0;
	for (int i=0;i<counter;i++){
		total=total+toys[i];
	}
	return total;
}
/*************************************************************************

    Function name:	totalToys
    Description:	Counts the total of the toys made
    Parameters:		toys - toys worker made
					counter - number for parallel array
    Return Value:	total - total for toys
 
*************************************************************************/

string findHigh(string worker[], int toys[], int counter){
	string high=worker[0];
	int tempHigh=toys[0];
	for (int i=0;i<counter;i++){
		if (toys[i]>tempHigh){
			tempHigh=toys[i];
			high=worker[i];
		}
	}
	return high;
}
/*************************************************************************

	Function name:	findHigh
	Description:	Finds the worker with highest toys made
	Parameters:		worker - name of worker
					toys - toys worker made
					counter - number for parallel array
	Return Value:	high - highest efficiency worker
 
*************************************************************************/

string findLow(string worker[], int toys[], int counter){
	string low=worker[0];
	int tempLow=toys[0];
	for (int i=0;i<counter;i++){
		if (toys[i]<tempLow){
			tempLow=toys[i];
			low=worker[i];
		}
	}
	return low;
}
/*************************************************************************

	Function name:	findLow
	Description:	Finds the worker with lowest toys made
	Parameters:		worker - name of worker
					toys - toys worker made
					counter - number for parallel array
	Return Value:	low - lowest efficiency worker
 
*************************************************************************/

void displayAnalysis(string worker[], int toys[], string rating[], int counter, int topWorker, int totalToys, string high, string low){
	cout<<endl;
	cout<<"----------------------------------------"<<endl;
	cout<<"   |Name|     |Toys Made|     |Ratings| "<<endl;
	cout<<"----------------------------------------"<<endl;
	for (int i=0;i<counter;i++){
		cout<<setw(12)<<worker[i]<<setw(10)<<toys[i]<<setw(15)<<rating[i]<<endl<<endl;
	}
	cout<<"----------------------------------------"<<endl<<endl;
	cout<<"Workers with highest rating (500+ made): "<<topWorker<<endl;
	cout<<"Total toys made: "<<totalToys<<endl;
	cout<<"Worker with highest toys made: "<<high<<endl;
	cout<<"Worker with highest toys made: "<<low<<endl<<endl;
	cout<<"Program complete."<<endl<<endl;
}
/*************************************************************************

    Function name:	displayAnalysis
    Description:	Displays all three arrays next to eachother
    Parameters:		worker - name of worker
					toys - toys worker made
					rating - rating which correlates with toys made
    Return Value:	None
 
*************************************************************************/
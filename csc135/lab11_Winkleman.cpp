/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 4/27/18
     Due Date:      4/30/18   
     Course:        CSC135-020 
     Professor:     Dr. Joo Tan 
     Assignment:    Lab 11
     Filename:      lab11_Winkleman.cpp
     Purpose:       A program used to test understanding of arrays.

******************************************************************/

#include <iostream>	//Basic library for input/output
#include <cstdlib>	//Used for "cerr" command
using namespace std;

//Prototypes
void intro(int&);
void enterGrades(float[], int);
void findOrder(float[], int);
void findOrderReverse(float[], int);
void calcHigh(float[], int);
void calcLow(float[], int);
float calcAverage(float[], int);
void aboveAverage(float[], int, float);


int main(){
	//Local variables
	int size;
	
	//Start program
	intro(size);
	float grade[size];
	enterGrades(grade, size);
	findOrder(grade, size);
	findOrderReverse(grade, size);
	calcHigh(grade, size);
	calcLow(grade, size);
	float avg=calcAverage(grade, size);
	aboveAverage(grade, size, avg);

return 0;}


void intro(int& size){
	cout<<endl;	//Description
	cout<<"--------------------------------------------------"<<endl;
	cout<<"This program is for calculating the average grade"<<endl;
	cout<<"of those that are entered among many other features"<<endl<<endl;
	cout<<"which will be seen upon completion."<<endl;
	cout<<"--------------------------------------------------"<<endl<<endl;
	
	cout<<"How many grades are you going to enter: ";	//User declaration of array size.
	cin>>size;
	cout<<endl;
}
/*************************************************************************

    Function name:	intro
    Description:	Introduction of program
    Parameters:		size - array size
    Return Value:	None
 
*************************************************************************/

void enterGrades(float grade[], int size){
	for(int i=0;i<size;i++){
		cout<<"Please enter a grade: ";
		cin>>grade[i];
		if (grade[i]<0){
			cerr<<"Please enter a valid value: ";
			cin>>grade[i];
		}
	}
	cout<<endl;
}
/*************************************************************************

    Function name:	enterGrades
    Description:	User input of grades
    Parameters:		grade - array holding entries
					size - counter for array entry
    Return Value:	None
 
*************************************************************************/

void findOrder(float grade[], int size){
	cout<<"Order entered: ";
	for(int i=0;i<size;i++){
		cout<<grade[i]<<" ";
	}
	cout<<endl;
}
/*************************************************************************

    Function name:	findOrder
    Description:	Displays order of numbers entered
    Parameters:		grade - array holding entries
					size - counter for array entry
    Return Value:	None
 
*************************************************************************/

void findOrderReverse(float grade[], int size){
	cout<<"Reverse order: ";
	for(int i=0;i<size;){
		size--;
		cout<<grade[size]<<" ";
	}
	cout<<endl;
}
/*************************************************************************

    Function name:	programfinished
    Description:	Displays reverse order of numbers entered
    Parameters:		grade - array holding entries
					size - counter for array entry
    Return Value:	None
 
*************************************************************************/

void calcHigh(float grade[], int size){
	float high=grade[0];
	cout<<"Highest grade: ";
	for(int i=0;i<size;i++){
		if (high<grade[i]){
			high=grade[i];
		}
	}
	cout<<high<<endl;
}
/*************************************************************************

    Function name:	calcHigh
    Description:	Outputs highest entry.
    Parameters:		grade - array holding entries
					size - counter for array entry
    Return Value:	None
 
*************************************************************************/

void calcLow(float grade[], int size){
	float low=grade[0];
	cout<<"Lowest grade: ";
	for(int i=0;i<size;i++){
		if (low>grade[i]){
			low=grade[i];
		}
	}
	cout<<low<<endl;
}
/*************************************************************************

    Function name:	calcLow
    Description:	Outputs lowest grade
    Parameters:		grade - array holding entries
					size - counter for array entry
    Return Value:	None
 
*************************************************************************/

float calcAverage(float grade[], int size){
	float avg=0;
	cout<<"Grade average: ";
	for(int i=0;i<size;i++){
		avg=avg+grade[i];
	}
	avg=avg/size;
	cout<<avg<<endl;
return avg;}
/*************************************************************************

    Function name:	calcAverage
    Description:	Outputs grade average.
    Parameters:		grade - array holding entries
					size - counter for array entry
    Return Value:	avg - grade average
 
*************************************************************************/

void aboveAverage(float grade[], int size, float avg){
	int abvAvg=0;
	cout<<"Grades above average: ";
	for(int i=0;i<size;i++){
		if (avg<grade[i]){
			abvAvg++;
		}
	}
	cout<<abvAvg<<endl<<endl;
}
/*************************************************************************

    Function name:	aboveAverage
    Description:	Outputs how many had higher grades than the average.
    Parameters:		grade - array holding entries
					size - counter for array entry
					avg - grade average
    Return Value:	None
 
*************************************************************************/
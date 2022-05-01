/************************************************************/
/* Author: Henry Winkleman 									*/
/* Creation Date: November 1, 2018 							*/
/* Due Date: November 7, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #3											*/
/* Filename: testArray.cpp 									*/
/* Purpose: Tests performance and functions of class Array.	*/
/* 															*/
/************************************************************/

#include <iostream>	//Cout/cin commands
#include <iomanip>	//Text commands
#include <cstdlib>	//Random values for testing
#include <ctime>	//Used for srand()
#include "Array.h"	//Header for array class

void intro();
void cycleOutput(const Array&,const int&,const int&,const string&);
void copyTestExplain();
void testDifferent(const Array&,const Array&);

using namespace std;

int main(){
	//Starting declarations and other necessary commands
	srand(time(NULL));				//Clears seed for randomization
	Array tester(rand()%6+4);		//Creates object with new capacity of 3-10.
	string testerName="Array #1",	//Names for use in cycle data
	testerDuplicateName="Array #2";
	
	//Program start
	intro();	//Explains test purpose.
	
	//First additions
	for(int idx=0, MAX=2;idx<MAX;idx++){
		int tempCap=tester.getCapacity(),	//Holds capacity to check if it has been raised
		cycleValue=rand()%100+1;			//Random value from 1-100
		tester+=cycleValue;					//Inputs random value
		cycleOutput(tester,cycleValue,tempCap,testerName);	//Displays all data in cycle
	}
	
	//Copy and equivalency testing
	copyTestExplain();						//Explains this test
	Array testerDuplicate=tester;			//Copies the Array
	testDifferent(tester,testerDuplicate);	//Tests if it was correctly copied
	
	for(int idx=2, MAX=3;idx<MAX;idx++){	//Adds a value to tester
		int tempCap=tester.getCapacity(),	//Holds capacity to check if it has been raised
		cycleValue=rand()%100+1;			//Random value from 1-100
		tester+=cycleValue;					//Inputs random value
		cycleOutput(tester,cycleValue,tempCap,testerName);	//Displays all data in cycle
	}
	for(int idx=2, MAX=3;idx<MAX;idx++){			//Adds a value to testerDuplicate
		int tempCap=testerDuplicate.getCapacity(),	//Holds capacity to check if it has been raised
		cycleValue=rand()%100+1;					//Random value from 1-100
		testerDuplicate+=cycleValue;				//Inputs random value
		cycleOutput(testerDuplicate,cycleValue,tempCap,testerDuplicateName);	//Displays all data in cycle
	}
	testDifferent(tester,testerDuplicate);	//Tests for equivalency after new values were added
	
	//Resume additions to tester until capacity is increased twice
	for(int idx=3, MAX=tester.getCapacity()+2;idx<MAX;idx++){
		int tempCap=tester.getCapacity(),	//Holds capacity to check if it has been raised
		cycleValue=rand()%100+1;			//Random value from 1-100
		tester+=cycleValue;					//Inputs random value
		cycleOutput(tester,cycleValue,tempCap,testerName);	//Displays all data in cycle
	}
	
	cout<<endl<<"Program complete."<<endl<<endl;	//Signifies completion
	return 0;
}

/*************************************************************************/
/*																		*/
/* Function name: 	intro												*/
/* Description: 	Explanation of program intention					*/
/* Parameters: 		None												*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void intro(){
	cout<<endl<<"************************************************"<<endl<<endl;
	cout<<"This program tests the array class in Array.cpp"<<endl;
	cout<<"and Array.h. It uses random values for input and"<<endl;
	cout<<"the array has a random capacity from 3-10. This"<<endl;
	cout<<"program will attempt to exceed the capacity and"<<endl;
	cout<<"force a copy to be made with a higher capacity"<<endl;
	cout<<"twice. It will then display all data,"<<endl;
	cout<<"including the values in the Array object along"<<endl;
	cout<<"with the value inputted and cycle number."<<endl<<endl;
	cout<<"A second test includes copying and testing"<<endl;
	cout<<"equivalency between two Array objects. Further"<<endl;
	cout<<"explaination is provided after cycle 2."<<endl<<endl;
	cout<<"************************************************"<<endl;
}

/*************************************************************************/
/*																		*/
/* Function name: 	cycleOutput											*/
/* Description: 	Displays all data for each cycle run.				*/
/* Parameters: 		tester/IMPORT - Array with value added,				*/
/*					cycleValue/IMPORT - The value added to the Array,	*/
/*					tempCap/IMPORT - Old capacity used to check if		*/
/*					capacity was increased in the process,				*/
/*					name/IMPORT - The given name for the Array used		*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void cycleOutput(const Array& tester,const int& cycleValue,const int& tempCap,const string& name){
	cout<<"------------------------------------------------"<<endl;
	
	cout<<"Array: "<<name<<endl;
	cout<<"Cycle:"<<setw(4)<<tester.getEltsInUse();
	cout<<"\t\tInput:"<<setw(4)<<cycleValue<<endl;
	cout<<"Array capacity:"<<setw(5)<<tester.getCapacity();
	
	if(tester.getCapacity()>tempCap)	//If capacity was raised
			cout<<"\t"<<"Old capacity:"<<setw(4)<<tempCap;
	
	cout<<endl;
	cout<<"Elements in use:"<<setw(4)<<tester.getEltsInUse()<<endl<<endl;
	cout<<tester;						//Default class output
	
	cout<<"------------------------------------------------"<<endl;
}

/*************************************************************************/
/*																		*/
/* Function name: 	copyTestExplain										*/
/* Description: 	Explains the copy and equivalency test				*/
/* Parameters: 		None												*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void copyTestExplain(){
	cout<<endl<<"------------------------------------------------"<<endl<<endl;
	cout<<"This section will test if Array #1 can be copied"<<endl;
	cout<<"and tested for equivalency before and after"<<endl;
	cout<<"further additions have been made to both arrays."<<endl<<endl;
	cout<<"Array #1 is copied as Array #2. Additions will"<<endl;
	cout<<"continue to Array #1 after the second test following"<<endl;
	cout<<"cycle 3."<<endl<<endl;
	cout<<"------------------------------------------------"<<endl;
}

/*************************************************************************/
/*																		*/
/* Function name: 	testDifferent										*/
/* Description: 	Tests if values in Array tester and testerDuplicate	*/
/*					are equal or not.									*/
/* Parameters: 		tester/IMPORT - First Array made, 					*/
/*					testerDuplicate/IMPORT - Copy of first Array		*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void testDifferent(const Array& tester,const Array& testerDuplicate){
	cout<<endl<<"------------------------------------------------"<<endl<<endl;
	cout<<"Testing for array equivalencies between Array #1"<<endl;
	cout<<"and Array #2."<<endl<<endl;
	if(tester!=testerDuplicate)
		cout<<"The arrays are found to be different."<<endl;
	else
		cout<<"The arrays are found to be the similar."<<endl;
	cout<<"Additions will continue."<<endl<<endl;
	cout<<"------------------------------------------------"<<endl<<endl;
}

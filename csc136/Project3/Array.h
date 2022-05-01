/************************************************************/
/* Author: Henry Winkleman 									*/
/* Creation Date: November 1, 2018 							*/
/* Due Date: November 7, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #3											*/
/* Filename: Array.h										*/
/* Purpose: Prototyping for functions in Array.cpp.			*/
/* Array - A class which holds a series of given numbers	*/
/* and outputs those numbers on lines four at a time.		*/
/* 															*/
/************************************************************/

#ifndef ARRAY_H
#define ARRAY_H

#include <iostream>

using namespace std;

class Array{

public:
	Array(int arrayCapacity= 10);	//Default constructor
	Array(const Array &init);		//Copy constructor
	~Array();						//Destructor
   
   //Sets and Gets
   
	//Set the capacity
	//Mutator - Passed int/IMPORT
	void setCapacity(int);
	
	//Sets eltsInUse value
	//Mutator - Passed int/IMPORT
	void setEltsInUse(int);
	
	//Return capacity
	//Inspector - Returned int/EXPORT
	int getCapacity() const;
	
	//Returns the number of elements in use
	//Inspector - Returned int/EXPORT
	int getEltsInUse() const;

	//Functions
	
	//Sorts the values in the array
	//Mutator - None
	void sort();

	//Overloaded Operators
	
	//Assign array values into another
	//Mutator - Passed Array/IMPORT
	const Array &operator=(const Array&);
	
	//Add a new value to the array
	//Mutator - Passed int/IMPORT
	Array operator+=(int);
	
	//Compare arrays
	//Facilitator - Passed Array/IMPORT,
	//				Returned bool/EXPORT
	bool operator==(const Array&) const;
	
	//Determine if two arrays are not equal
	//Facilitator - Passed Array/IMPORT,
	//				Returned bool/EXPORT
	bool operator!=(const Array &right) const;

	//l-value subscript operator
	//Inspector - Passed int/IMPORT,
	//				Returned int/EXPORT
	int &operator[](int);
	
	//r-value subscript operator
	//Inspector - Passed int/IMPORT,
	//				Returned int/EXPORT
	const int &operator[](int) const;
	
	// Return count of arrays instantiated.
	//Inspector - Returned int/EXPORT
	static int getArrayCount();

private:
	int capacity; 		//Capacity of the array
	int eltsInUse;		//Elements in the array in use
	int *ptr;			//Pointer to first element of array
	static int arrayCount;	//# of Arrays instantiated
};
	
	//Gives a specific format of output
	//Passed stream/IMPORT, passed Array//IMPORT
	//Returned stream output/EXPORT
	ostream &operator<<(ostream &, const Array &);

#endif

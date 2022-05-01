/************************************************************/
/* Author: Henry Winkleman 									*/
/* Creation Date: November 1, 2018 							*/
/* Due Date: November 7, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #3											*/
/* Filename: Array.cpp 										*/
/* Purpose: Holds commands for class Array - A class which	*/
/* holds a series of given numbers and outputs those numbers*/
/* on lines four at a time.									*/
/* 															*/
/************************************************************/

#include <iostream>	//Stream commands
#include <iomanip>	//Used for formatting output
#include <assert.h>	//Assert command
#include "Array.h"	//Array class header
#include "SortSearch.h"	//selSort() function in header

using namespace std;

//Initialize static data member at file scope
int Array::arrayCount=0;	//No objects yet

//Default constructor for class Array (default capacity 10)
Array::Array(int arrayCapacity){
	setCapacity((arrayCapacity>0?arrayCapacity:10));
	setEltsInUse(0);			//Default elts set to 0
	ptr=new int[getCapacity()];	//Create space for array
	assert(ptr!=0);				//Terminate if memory not allocated
	++arrayCount;				//Count one more object
}

//Copy constructor for class Array
//Must receive a reference to prevent infinite recursion
Array::Array(const Array &init){
	setCapacity(init.getCapacity());	//Copies capacity value
	setEltsInUse(init.getEltsInUse());	//Copies number of elements value
	ptr=new int[getCapacity()];			//Create space for array
	assert(ptr!=0);						//Terminate if memory not allocated
	++arrayCount;						//Count one more object

	for (int idx=0;idx<getEltsInUse();idx++)
		ptr[idx]=init.ptr[idx];			//Copy init elements into object
}

//Destructor for class Array
Array::~Array(){
	delete [] ptr;	//Reclaim space for array
	--arrayCount;	//One fewer objects
}

//Set the Array's capacity
void Array::setCapacity(int elts){
	capacity=elts;	//Capacity equivalent to number passed.
}

//Set the array's number of elements in use
void Array::setEltsInUse(int elts){
	eltsInUse=elts;	//eltsInUse equivalent to number passed.
}

// Get the capacity of the array
int Array::getCapacity() const{
	return capacity;	//Returns capacity value.
}

//Get the number of elements in use in the array
int Array::getEltsInUse() const{
	return eltsInUse;	//Returns eltsInUse value.
}

//Uses SortSearch function to sort all values in the array
void Array::sort(){
	selSort(ptr, getEltsInUse());	/*Calls on sort function for the array,
									runs for each element.*/
}

//Overloaded assignment operator
//const return avoids: (a1=a2)=a3
const Array &Array::operator=(const Array &right){
	if(&right!=this){	//Check for self-assignment
	
		//For arrays of different capacities, deallocate original
		//Left side array, then allocate new left side array.
		if(getCapacity()!=right.getCapacity()){
			delete [] ptr;						//Reclaim space
			setCapacity(right.getCapacity());	//Resize this object
			ptr=new int[getCapacity()];			//Create space for array copy
			assert(ptr!=0);						//Terminate if not allocated
		}

	for(int idx=0;idx<getEltsInUse();idx++)
		ptr[idx]=right[idx];	//Copy elements from array into object
	}

	return *this;	//Enables x=y=z;
}

/*Throws passed value into the array.
Creates larger array if capacity was reached
before insertion. Returns self.*/
Array Array::operator+=(int newValue){
	if(getEltsInUse()>=getCapacity()){	//If elts has reached the capacity
		setCapacity(getCapacity()+1);	//Increase capacity by 1
		int* tempPtr=new int[getCapacity()];	//Create array with new capacity
		for (int idx=0;idx<getEltsInUse();idx++)
			tempPtr[idx]=ptr[idx];		//Copy all elements
		delete [] ptr;					//Delete old array
		ptr=tempPtr;					//Object now points to larger array
	}
	ptr[getEltsInUse()]=newValue;	//Place value in next availiable spot
	setEltsInUse(getEltsInUse()+1);	//Raise number of elements by 1
	sort();							//Sort array
	
	return *this;
}

//Determine if two arrays are equal and
//return true, otherwise return false.
bool Array::operator==(const Array &right) const{
	if(getCapacity()!=right.getCapacity())
		return false;	//Arrays of different capacities

	for(int idx=0;idx<getEltsInUse();idx++)
		if (ptr[idx]!=right[idx])
			return false;	//Arrays are not equal

	return true;	//Arrays are equal
}

//Determine if two arrays are not equal and
//return true, otherwise return false (uses operator==).
bool Array::operator!=(const Array &right) const{
	return!(*this==right);
}

//Overloaded subscript operator for non-const Arrays
//Reference return creates an lvalue
int &Array::operator[](int subscript){
	
	//Check for subscript out of range error
	assert(0<=subscript&&subscript<getEltsInUse());

	return ptr[subscript];	//Reference return
}

//Overloaded subscript operator for const Arrays
//const reference return creates an rvalue
const int &Array::operator[](int subscript) const{

	//Check for subscript out of range error
	assert(0<=subscript&&subscript<getEltsInUse());

	return ptr[subscript];	//const reference return
}

//Return the number of Array objects instantiated
//Static functions cannot be const 
int Array::getArrayCount(){
	return arrayCount;
}

//Overloaded output operator for class Array 
ostream &operator<<(ostream &output, const Array &a){
	int idx;
	for(idx=0;idx<a.getEltsInUse();idx++){	//For every element
		output<<setw(12)<<a[idx];	//12 spaces for the number

		if((idx+1)%4==0) 			//4 numbers per row of output
			output<<endl;
	}

	if(idx%4!=0)	/*Creates end line for last value 
					if it is not divisible by 4.*/
		output<<endl;

	return output;	//Enables cout<<x<<y;
}

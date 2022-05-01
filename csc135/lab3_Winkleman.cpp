/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 2/14/18
     Due Date:      2/14/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Lab 3
     Filename:      lab3_Winkleman.cpp
     Purpose:       A lab testing my knowledge of the use of equations.

******************************************************************/

#include <iostream>
using namespace std;

int main() {
	
	//variable declaration
	string name;
	float height;
	float bmi;
	float weight;

	//input
	
	cout << "Please enter your name:";
	getline(cin, name);
	cout << "Please enter your height:";
	cin >> height;
	cout << "Please enter your weight:";
	cin >> weight;
	
	//conversion
	
	bmi = (703*weight)/(height*height);
	
	//output
	
	cout << "Hello, " << name << "." << endl;
	cout << "Your weight is " << weight << " pounds.";
	cout << "Your height is " << height << " inches.";
	cout << "Your body mass index is " << bmi << "." << endl;
	
	return 0;
	
}
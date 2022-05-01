/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 2/9/18
     Due Date:      2/9/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Lab 2
     Filename:      lab2_Winkleman.cpp
     Purpose:       A lab testing my knowledge of the use of variables and literals

******************************************************************/

#include <iostream>
using namespace std;

int main() {
	
	int number = 23;
	cout << "There are " << number << " students in my class." << endl ;
	int num1 = 10;
	int num2 = 15;
	int num3 = 20;
	num1 = num1 + 20;
	num3 = num1 + num2;
	cout << num1 << " " << num2 << " " << num3 << endl ;
	string my_name = "Henry Winkleman";
	cout << "Hello, I am " << my_name << ". It is nice to meet you." << endl ;
	char grade = 'A';
	cout << "My grade for this class is " << grade << "." << endl ;
	int random;
	cout << random << endl ;

	return 0;
	
}
/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 3/2/18
     Due Date:      3/2/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Lab 5
     Filename:      lab5Temperature_Winkleman.cpp
     Purpose:       A lab to test my understanding of the if/else command.

******************************************************************/

#include <iostream>
using namespace std;

int main() {

//Declaration
float C;
float F;

//Input
cout << "Please enter the temperature for today in Celsius: ";
cin >> C;

//Conversion
F=((C*1.8)+32);
cout << "The temperature today is " << F <<"\u00B0" << "F." << endl;

//If/Else Output
if (F>=86)
{
	cout << "It is hot today." << endl;
}
else if (F<86&&F>=59)
{
	cout << "It is warm today." << endl;
}
else if (F<59&&F>=32)
{
	cout << "It is cold today." << endl;
}
else
{
	cout << "It is freezing today." << endl;
}

	return 0;
	
}
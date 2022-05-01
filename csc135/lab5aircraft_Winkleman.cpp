/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 3/2/18
     Due Date:      3/2/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Lab 5
     Filename:      lab5aircraft_Winkleman.cpp
     Purpose:       A lab to test my understanding of the if/else command.

******************************************************************/

#include <iostream>
using namespace std;

int main() {

//Declaration
float kmh;
float m;
int years;

//Input
cout << "Please enter observed speed in kilometers/hour: ";
cin >> kmh;
if (kmh>1100)
{
	cout << "Please enter the estimated length in meters: ";
	cin >> m;
	if (m>52)
		cout << "Civilian" << endl;
	else
		cout << "Military" << endl;
}
else
{
	cout << "Please enter the years it has been in service: ";
	cin >> years;
	if (years>7)
		cout << "Civilian" << endl;
	else
		cout << "Military" << endl;
}

	return 0;
	
}
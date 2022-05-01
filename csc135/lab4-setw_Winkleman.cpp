/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 2/19/18
     Due Date:      2/19/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Lab 4 Setw
     Filename:      lab4-setw_Winkleman.cpp
     Purpose:       A lab testing my knowledge of the use of formatting.

******************************************************************/

#include <iostream>
#include <iomanip>
using namespace std;

int main() {

//41 space
//11
cout << "Movie Name:" << setw(33) << "Shrek 3" << endl;
//20
cout << "Adult Tickets Sold:" << setw(21) << "500" << endl;
//20
cout << "Child Tickets Sold:" << setw (21) << "275" << endl;

float grossp = 6737.50;
float netp = 1347.50;

cout << "Gross Box Office Profit:" << setw(14) << "$" << setprecision(2) << fixed << grossp << endl;
cout << "Net Box Office Profit:" << setw(16) << "$" << setprecision(2) << fixed << netp << endl;

	return 0;
	
}
/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 2/14/18
     Due Date:      2/14/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Project 1
     Filename:      proj1_Winkleman.cpp
     Purpose:       A calculator created to determine the interest generated over a period of time.

******************************************************************/

#include <iostream>
#include <iomanip>
#include <math.h>
using namespace std;

int main() {
	/*A - Assign
	I - Input
	F - Formula
	O - Output*/
	
	/*A:Variable
	Declaring variable types for use in the input section.*/
	float balance;
	float irate;
	int compound;
	float interest;
	float total;

	/*I:User Input
	Allows the user to assign values to the variables for use in the calculation.*/
	cout << "Hello, user." << endl;
	cout << "This is a program made to calculate monatary interest over one year." << endl;
	cout << "Please enter values." << endl;
	cout << "Principal (Savings Balance): ";
	cin >> balance;
	cout << "Interest rate (in decimals): ";
	cin >> irate;
	cout << "Times compounded over a year: ";
	cin >> compound;
	
	/*F:Interest Calculator
	Using the formula for compound interest, it will determine what interest is accumulated.*/
	total = (balance*pow((1+(irate/compound)), compound));
	interest = (total-balance);
	/*F:Decimal to Percent Conversion
	For use in output.*/
	irate = irate*100;
	
	/*O:Calculator Output -
	Displays the values entered by the user and the value generated through the calculation.
	Formatted to 25 spaces.*/
	cout << "Interest Rate:" /*14*/ << setw(10) << " " << fixed << setprecision(2) << irate << "%" << endl;
	cout << "Times Compounded:" /*17*/<< setw(7) << " " << compound << endl;
	cout << "Principal:" /*10*/<< setw(15) << "$" << setw(8) << setprecision(2) << fixed << balance << endl;
	cout << "Interest:" /*9*/<< setw(16) << "$" << setw(8) << setprecision(2) << fixed << interest << endl;
	cout << "Amount in Savings:" /*18*/<< setw(7) << "$" << setw(8) << setprecision(2) << fixed << total << endl;
	
	return 0;
	
}
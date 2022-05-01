/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 4/9/18
     Due Date:      4/9/18   
     Course:        CSC135-020 
     Professor:     Dr. Joo Tan 
     Assignment:    Project 4
     Filename:      proj4_Winkleman.cpp
     Purpose:       A program used to calculate the average of ten grades for multiple people.

******************************************************************/

#include <iostream>
#include <iomanip>
using namespace std;

//Prototypes
void welcome();
string entername();
void entergrade(int&, int&, int&, int&, int&, int&, int&, int&, int&, int&);
float calcavg(int, int, int, int, int, int, int, int, int, int);
char calcletter(float);
void output(string, float, char);

int main(){
	welcome(); //Introduction
	string name;
	int gr1, gr2, gr3, gr4, gr5, gr6, gr7, gr8, gr9, gr10;
	while (name != "done"){ //Starts program function loop
		name=entername(); //Enter student name
		if (name != "done"){ //Checks for sentinel "done"
			entergrade(gr1, gr2, gr3, gr4, gr5, gr6, gr7, gr8, gr9, gr10); //Grades entered
			float avg=calcavg(gr1, gr2, gr3, gr4, gr5, gr6, gr7, gr8, gr9, gr10); //Average calculated of grades entered
			char letter=calcletter(avg); //Letter conversion
			output(name, avg, letter);} //Final output of all calculated inputs
		else //Sentinel triggers a break from the loop
			break;}
return 0;}

//Functions
/*************************************************************************

    Function name: 	welcome
    Description: 	Prints welcome text for the program and lists its functions.
    Parameters: 	None.
    Return Value: 	None.
 
*************************************************************************/
void welcome(){
	cout<<"----------------------------------------------------------------------"<<endl;
	cout<<"Enter the student's name along with ten grades."<<endl;
	cout<<"This program will calculate and output the average and letter grade."<<endl;
	cout<<"Enter 'done' in the name entry when finished."<<endl;
	cout<<"----------------------------------------------------------------------"<<endl<<endl;}

/*************************************************************************

    Function name: 	entername
    Description: 	User input for student name.
    Parameters: 	None
    Return Value: 	tempname - String for student's name.
 
*************************************************************************/
string entername(){
	string tempname;
	cout<<"Student's name: ";
	getline(cin, tempname);
return tempname;}

/*************************************************************************

    Function name: 	entergrade
    Description: 	User input for the ten grades.
    Parameters: 	gr1-gr10 - Ten slots for the grades to be entered.
    Return Value: 	None
 
*************************************************************************/
void entergrade(int &gr1, int &gr2, int &gr3, int &gr4, int &gr5, int &gr6, int &gr7, int &gr8, int &gr9, int &gr10){
	cout<<"Grades: ";
	cin>>gr1>>gr2>>gr3>>gr4>>gr5>>gr6>>gr7>>gr8>>gr9>>gr10;}

/*************************************************************************

    Function name: 	calcavg
    Description: 	Finds the average of the ten grades.
    Parameters: 	gr1-gr10 - Ten slots for the grades to be entered.
    Return Value: 	tempavg - Average for the ten grades entered.
 
*************************************************************************/
float calcavg(int gr1, int gr2, int gr3, int gr4, int gr5, int gr6, int gr7, int gr8, int gr9, int gr10){
	float total=gr1+gr2+gr3+gr4+gr5+gr6+gr7+gr8+gr9+gr10;
	float tempavg=total/10;
return tempavg;}
	
/*************************************************************************

    Function name: 	calcletter
    Description: 	converts the average into a letter grade.
    Parameters: 	avg - Average grade.
    Return Value: 	templetter - Conversion of average to a letter.
 
*************************************************************************/
char calcletter(float avg){
	char templetter;
	if (avg>=90)
		templetter='A';
	else if (avg>=80&&avg<90)
		templetter='B';
	else if (avg>=70&&avg<80)
		templetter='C';
	else if (avg>=60&&avg<70)
		templetter='D';
	else
		templetter='F';
return templetter;}

/*************************************************************************

    Function name: 	output
    Description: 	Prints the service type, title, and cost.
    Parameters: 	name - Student name
					avg - Average grade
					letter - Letter grade
    Return Value: 	None. 
 
*************************************************************************/
void output(string name, float avg, char letter){
	cout<<endl;
	cout<<"Name: "<<setw(14)<<" "<<name<<endl;
	cout<<"Average grade: "<<setw(5)<<" "<<setprecision(1)<<fixed<<avg<<endl;
	cout<<"Letter grade: "<<setw(6)<<" "<<letter<<endl<<endl<<endl;
	cin.ignore(1000,'\n');}
/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 4/1/18
     Due Date:      4/2/18   
     Course:        CSC135-020 
     Professor:     Dr. Joo Tan 
     Assignment:    Project 3
     Filename:      proj3_Winkleman.cpp
     Purpose:       A program used to calculate cost of on-demand entertainment.

******************************************************************/

#include <iostream>
#include <iomanip>
using namespace std;

//Prototypes
void welcome();
void servicechoice(char &, float &);
string servicename();
void moviedate(float &);
void order(char, string, float);

int main(){
	char servicetype;
	float cost;
	welcome(); //Shows key for service costs and types.
	servicechoice (servicetype, cost); //User input for type of entertainment.
	switch (servicetype){ /* Determines if movie year is necessary for the cost */
		case 'm':
		moviedate (cost); break;
		case 'M':
		moviedate (cost); break;
		default:
		break;}
	string title=servicename(); //User input for title of entertainment.
	order (servicetype, title, cost); //Prints the order reciept.
return 0;}

//Functions
void welcome() {
	cout<< endl << "-----------------------------------------"<<endl;
	cout<< "Welcome to CNR Cable On-Demand!" << endl;
	cout<<"TV shows (code t/T) cost $0.00" <<endl;
	cout<<"Newly-released movies (code n/N) cost $6.99." <<endl;
	cout<<"Movies (code m/M) cost $5.99 or less." <<endl;
	cout<<"-----------------------------------------"<<endl<<endl;}
/*************************************************************************

    Function name: 	welcome
    Description: 	Prints welcome text for the on-demand service.
    Parameters: 	None.
    Return Value: 	None.
 
*************************************************************************/

void servicechoice(char &servicetype, float &cost){
	cout << "What type of entertainment do want: ";
	cin >> servicetype;
	if (servicetype=='t'||servicetype=='T'){
		cout<<"You have chosen a TV show."<<endl;
		cost=0.00;}
	else if (servicetype=='n'||servicetype=='N'){
		cout<<"You have chosen a newly-released movie."<<endl;
		cost=6.99;}
	else if (servicetype=='m'||servicetype=='M'){
		cout<<"You have chosen a movie."<<endl;
		cost=5.99;}}
/*************************************************************************

    Function name: 	servicechoice
    Description: 	User input for entertainment type and title.
    Parameters: 	servicetype - The type of entertainment ordered.
					cost - Price of entertainment
    Return Value: 	None
 
*************************************************************************/

string servicename(){
	string title;
	cout<<"Name of show or movie:"<<endl;
	cin.ignore(1000,'\n');
	getline(cin, title);
	return title;}
/*************************************************************************

    Function name: 	servicename
    Description: 	User input for title of movie or show.
    Parameters: 	None
    Return Value: 	None
 
*************************************************************************/

void moviedate(float &cost){
	int year;
	cout<< "What year was the film produced: ";
	cin>> year;
	if (year>=2000)
		cost= 5.99;
	else if (year>=1980&&year<2000)
		cost= 4.99;
	else if (year>=1960&&year<1980)
		cost= 3.99;
	else if (year<1960)
		cost= 2.99;}
/*************************************************************************

    Function name: 	moviedate 
    Description: 	Finds cost based on movie year.
    Parameters: 	cost - Price of entertainment
    Return Value: 	None
 
*************************************************************************/

void order(char type, string title, float cost){
	string advtype;
	switch (type){
		case 't':
		advtype="TV"; break;
		case 'T':
		advtype="TV"; break;
		case 'n':
		advtype="New Movie"; break;
		case 'N':
		advtype="New Movie"; break;
		case 'm':
		advtype="Movie"; break;
		case 'M':
		advtype="Movie"; break;}
	cout<<endl;
	cout<<"================|Service Order|================"<<endl;
	cout<<"Entertainment Type:"<<setw(11)<<" "<<advtype<<endl;
	cout<<"Title:"<<setw(24)<<" "<<title<<endl;
	cout<<"Order Cost:"<<setw(20)<<"$"<<setw(8)<<setprecision(2)<<fixed<<cost<<endl<<endl;}
/*************************************************************************

    Function name: 	order
    Description: 	Prints the service type, title, and cost.
    Parameters: 	type - Type of entertainment ordered.
					title - The name of film or show.
					cost - Price of entertainment
    Return Value: 	None. 
 
*************************************************************************/
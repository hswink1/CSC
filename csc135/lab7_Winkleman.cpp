/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 4/25/18
     Due Date:      4/25/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Lab 7
     Filename:      lab7_Winkleman.cpp
     Purpose:       A lab to test my understanding of arrays.

******************************************************************/

#include <iostream>
using namespace std;

int main(){

//Local variables
int amt,grades[amt],i,high;

//Start
cout<<endl<<"----------------------------------------"<<endl;
cout<<"Please enter 'x' amount of grades and"<<endl;
cout<<"their values."<<endl;
cout<<"----------------------------------------"<<endl<<endl;

cout<<"Enter the number of grades: ";
cin>>amt;

for(i=0;i<amt;i++){
cout<<"Please enter value for grade "<<i+1<<": ";
cin<<grades[i];
}

high=grades[0]
for(i=0;i<amt;i++){
	if(high<grades[i]{
		high=grades[i];
	}
}

cout<<high;
return 0;}
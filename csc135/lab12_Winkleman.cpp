/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 5/2/18
     Due Date:      5/2/18   
     Course:        CSC135-020 
     Professor:     Tan 
     Assignment:    Lab 12
     Filename:      lab12_Winkleman.cpp
     Purpose:       A lab testing my knowledge of 2D arrays

******************************************************************/

#include <iostream>
using namespace std;

int main(){
	int row=4;
	int col=5;
	int array[row][col];
	
	array[0][0]=1;
	
	for (int i=0;i<row;i++){
		for (int j=1;j<col;j++){
			array[i][j]=array[i][j-1]*2;
		}
		array[i+1][0]=array[i][0]+1;
	}
	
	for (int k=0;k<row;k++){
		cout<<endl;
		for (int l=0;l<col;l++){
			cout<<array[k][l]<<" ";
		}
	}
	cout<<endl<<endl;
	
	return 0;
}
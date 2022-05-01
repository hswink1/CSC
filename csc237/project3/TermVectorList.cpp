/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 10, 2019 	   		  		              */
/* Due Date: November 13, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #3										                      	*/
/* Filename: TermVectorList.cpp 							           	  */
/* Purpose: This class reads from file and holds Terms in an*/
/*          Vector container to be printed and evaluated as */
/*          polynomials.                                    */
/************************************************************/

#include <fstream>
#include <iostream>
#include <iomanip>
#include "TermVectorList.h"
#include <chrono>

using namespace std;

/*********************************************************************
*   Function name:  TermVectorList() - constructor
*   Description:   	Allows for a TermVectorList to be created.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
  TermVectorList::TermVectorList(){}

  /*********************************************************************
  *   Function name:  readIntoList
  *   Description:   	All data from file is extracted as a term and held in
  *                    the specified container.
  *   Parameters:  		filename - IMPORT - File used for stream extraction.
  *
  *   Return Value: 	N/A
  *********************************************************************/
  void TermVectorList::readIntoList(string filename){
    ifstream source(filename.c_str());  //Opens the filestream
    double coeff;                       //Holds extracted coefficient
    int expn;                           //Holds extracted exponent
    bool placed=false;      //States if a term has been added to one in the list
    bool firstTerm=true;    //States if it is the first term to be added
    vector<Term>::iterator VecItr=ThePoly.begin();  //Starts iterator
    while (source >> coeff >> expn){  //While the stream is not empty,
      Term T(coeff,expn);             //Holder for the extraced data
      if(firstTerm==true){      //If it is the first time running,
        ThePoly.push_back(T);   //Insert the data
        firstTerm=false;        //Change the first time flag
      }
      else{
        //For all indexes, compare their exponent to the extracted one.
        for (int idx=0; idx<ThePoly.size()&&placed==false; idx++){
          if (ThePoly[idx]==T){ //If the extracted has the same exponent,
            //Make a Term with both coeffs added together
            Term Temp(ThePoly[idx].getCoefficient()+coeff, expn);
            ThePoly.push_back(Temp);  //Destroy the old Term
            ThePoly.erase(ThePoly.begin()+idx); //Place the new Term in
            placed=true;  //Flag that the Term was placed
          }
        }
        if(placed==false){      //If the Term has not already been placed,
          ThePoly.push_back(T); //Add it to the end.
        }
        placed=false; //Reset the placement flag.
      }
    }
    source.close(); //Close the stream
    source.clear(); //Clear the end flag for the stream
    sort(ThePoly.begin(), ThePoly.end()); //Sort the vector in ascending order
  }

/*********************************************************************
*   Function name:  printIteratively
*   Description:   	All the Terms in the list are printed in polynomial form
*                   in iteration.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
  void TermVectorList::printIteratively(){
    using namespace std::chrono;  //Allows use of std::chrono commands.
    auto begin=high_resolution_clock::now();  //Finds start time.
    cout<<"---------------------------------"<<endl;
    cout<<"|Vector  Iterative              |"<<endl;
    cout<<"---------------------------------"<<endl;
    //Create an iterator at the beginning of the list
    vector<Term>::iterator VecItr=ThePoly.begin();
    //Create an iterator at the end of the list
    vector<Term>::iterator VecItrBack=ThePoly.end();
    --VecItrBack;

    //Until the back iterator reaches the front
    while(VecItrBack!=VecItr){
      cout << *VecItrBack << " + "; //Output the Terms
      --VecItrBack;                 //Move the iterator further to the front
    }
    cout<<*VecItrBack<<endl<<endl;  //Special output for the last Term
    auto end=high_resolution_clock::now();
    auto ticks=duration_cast<microseconds>(end-begin);
    cout<<"It took "<<ticks.count()<<" microseconds to print this."<<endl<<endl;
}

/*********************************************************************
*   Function name:  printRecursively
*   Description:   	All the Terms in the list are printed in polynomial form
*                   through recursion.
*   Parameters:  		count - IMPORT/EXPORT - holds the amount of times the
*                   recursion was executed.
*
*   Return Value: 	N/A
*********************************************************************/
void TermVectorList::printRecursively(int count){
  int wasCount=count;   //Holds the count at the beginning of execution.
  using namespace std::chrono;  //Allows use of std::chrono commands.
  auto begin=high_resolution_clock::now();  //Finds start time.
  if(count==0){
    cout<<"---------------------------------"<<endl;
    cout<<"|Vector  Recursive              |"<<endl;
    cout<<"---------------------------------"<<endl;
  }
  //Create an iterator ath the beginning of the list
  vector<Term>::iterator VecItr=ThePoly.begin();
  //Create an iterator at the end of the list
  vector<Term>::iterator VecItrBack=ThePoly.end();
  --VecItrBack; //Set the iterator before the NULL index.
  for(int i=0;i<count;i++){ //For every time the recursion was run,
    --VecItrBack;           //Move the iterator back once more.
  }

  //Until the back iterator reaches the front
  if(VecItrBack!=VecItr){
    cout << *VecItrBack << " + "; //Output the Terms
    count++;                      //Increase the count for recursion
    printRecursively(count);      //Start recursion
  }
  else{
    cout<<*VecItrBack<<endl<<endl;  //Special output for the last Term
  }
  if(wasCount==0){  //For the initial recursion (conditional maybe not needed),
    auto end=high_resolution_clock::now();  //Calculate end time for print,
    auto ticks=duration_cast<microseconds>(end-begin);  //Calculate time to print.
    cout<<"It took "<<ticks.count()<<" microseconds to print this."<<endl<<endl;
  }
}

/*********************************************************************
*   Function name:  Overloaded operator - (x)
*   Description:   	All the Terms in the list are evaluated and totaled
*                   with the user input for 'x'.
*   Parameters:  		N/A
*
*   Return Value: 	double - the total of all the Terms
*********************************************************************/
  double TermVectorList::operator()(double x) const
  { double result=0.0;  //Result is held as a double
    for (int idx=0; idx<ThePoly.size(); idx++){ //For all indexes,
      result+=ThePoly.at(idx)(x);               //Add the evaluated polynomials
    }
    return(result);                             //Return the total
  }

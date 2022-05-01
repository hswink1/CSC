/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 10, 2019 	   		  		              */
/* Due Date: November 13, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #3										                      	*/
/* Filename: TermArrayList.cpp 							           	    */
/* Purpose: This class reads from file and holds Terms in an*/
/*          Array container to be printed and evaluated as  */
/*          polynomials.                                    */
/************************************************************/

#include <fstream>
#include <iostream>
#include <iomanip>
#include "TermArrayList.h"
#include <chrono>

using namespace std;

/*********************************************************************
*   Function name:  TermArrayList() - constructor
*   Description:   	Allows for a TermArrayList to be created.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
  TermArrayList::TermArrayList(){
    numTerms=0; //Number of terms is set to 0
  }

/*********************************************************************
*   Function name:  readIntoList
*   Description:   	All data from file is extracted as a term and held in
*                   the specified container.
*   Parameters:  		filename - IMPORT - File used for stream extraction.
*
*   Return Value: 	N/A
*********************************************************************/
  void TermArrayList::readIntoList(string filename){
    ifstream source(filename.c_str());  //The filestream is opened
    double coeff;                       //Holds extracted coefficient
    int expn;                           //Holds extracted exponent
    bool placed=false;                  //Flag for whether the Term has already
                                        //been placed

    while((source>>coeff>>expn)){    //If the file is not empty,
      Term T(coeff,expn);          //Extract the line into a Term
      for (int scanIdx=0; scanIdx<numTerms; scanIdx++){ //For every index,
        if (ThePoly[scanIdx]==T){//If the index has the same exponent as the Term
          //Make a new Term fusing the coefficients
          Term Temp((T.getCoefficient()+ThePoly[scanIdx].getCoefficient()), T.getExponent());
          ThePoly[scanIdx]=Temp;  //Overwrite the one already in the array
          placed=true;            //Flag that the Term has been placed
        }
      }
      if(placed==false&&numTerms!=10){      //If the Term has not been placed,
        ThePoly[numTerms++]=T;//Insert it in the list and increase the numTerms
      }
      placed=false; //Reset placement flag
    }
    source.close(); //Close the filestream
    source.clear(); //Clear the flag for the filestream

    //BUBBLE SORT, REQUIRES A PASS WITHOUT ANY SWAPS
    bool sorted=false;  //Flag to confirm the sort has worked
    while(sorted==false){ //If the array is not sorted,
      sorted=true;        //Set the flag to sorted
      for(int idx=0; idx<numTerms-1; idx++){  //For all indexes,
        if(ThePoly[idx]<ThePoly[idx+1]){    //If they are less than the next one
          swap(ThePoly[idx], ThePoly[idx+1]); //Swap the indexes
          sorted=false;                     //Flag that the array wasn't sorted
        }
      }
    }
  }

/*********************************************************************
*   Function name:  printIteratively
*   Description:   	All the Terms in the list are printed in polynomial form
*                   in iteration.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
  void TermArrayList::printIteratively(){
    using namespace std::chrono;  //Allows use of std::chrono commands.
    auto begin=high_resolution_clock::now();  //Finds start time.
    cout<<"---------------------------------"<<endl;
    cout<<"|Object  Array  Iterative       |"<<endl;
    cout<<"---------------------------------"<<endl;
    for(int i=0; i<(numTerms-1); i++) //For all but the last index,
      cout << ThePoly[i] << " + ";    //Output the Term
    cout << ThePoly[numTerms-1] << endl << endl;  //Special output for last Term
    auto end=high_resolution_clock::now();  //Calculate end time for print,
    auto ticks=duration_cast<microseconds>(end-begin);  //Calculate time to print.
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
  void TermArrayList::printRecursively(int count){
    int wasCount=count;   //Holds the count at the beginning of execution.
    using namespace std::chrono;  //Allows use of std::chrono commands.
    auto begin=high_resolution_clock::now();  //Finds start time.
    if(count==0){
      cout<<"---------------------------------"<<endl;
      cout<<"|Object  Array  Recursive       |"<<endl;
      cout<<"---------------------------------"<<endl;
    }
    if(count<numTerms-1){ //If the end of the list is not met,
      cout<<ThePoly[count]<<" + ";    //Output the Term
      count++;                        //Increase recursion count
      printRecursively(count);        //Start recursion
    }
    else if(count==numTerms-1){       //For the last term
      cout<<ThePoly[count]<<endl<<endl;  //Special output for last Term
    }
    if(wasCount==0){  //For the initial recursion (conditional maybe not needed),
      auto end=high_resolution_clock::now();  //Calculate end time for print,
      auto ticks=duration_cast<microseconds>(end-begin);  //Calculate time to print.
      cout<<"It took "<<ticks.count()<<" microseconds to print this."<<endl<<endl;
    }
  }

/*********************************************************************
*   Function name:  printPtr
*   Description:   	All the Terms in the list are printed in polynomial form
*                   by a pointer.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
  void TermArrayList::printPtr(){
    using namespace std::chrono;  //Allows use of std::chrono commands.
    auto begin=high_resolution_clock::now();  //Finds start time.
    cout<<"---------------------------------"<<endl;
    cout<<"|Object  Array  Pointer         |"<<endl;
    cout<<"---------------------------------"<<endl;
    Term *current=ThePoly;  //Create a pointer to print
    for(int i=0; i<(numTerms-1); i++) //For all but the last index,
      cout << *(current++) << " + ";  //The Term is printed by a pointer
    cout << *(current++) << endl << endl; //Special output for last Term
    auto end=high_resolution_clock::now();  //Calculate end time for print,
    auto ticks=duration_cast<microseconds>(end-begin);  //Calculate time to print.
    cout<<"It took "<<ticks.count()<<" microseconds to print this."<<endl<<endl;
  }

/*********************************************************************
*   Function name:  Overloaded operator - (x)
*   Description:   	All the Terms in the list are evaluated and totaled
*                   with the user input for 'x'.
*   Parameters:  		N/A
*
*   Return Value: 	double - the total of all the Terms
*********************************************************************/
  double TermArrayList::operator()(double x) const{
    double result=0.0;  //The result is held as a double
    for (int idx=0;idx<numTerms;idx++)  //For all indexes
      result+=ThePoly[idx](x);      //Add the value of the polynomials with 'x'
    return(result);                 //Return the total
  }

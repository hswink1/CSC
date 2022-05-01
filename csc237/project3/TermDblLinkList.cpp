/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 10, 2019 	   		  		              */
/* Due Date: November 13, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #3										                      	*/
/* Filename: TermDblLinkList.cpp 							         	    */
/* Purpose: This class reads from file and holds Terms in an*/
/*          DblLink container to be printed and evaluated as*/
/*          polynomials.                                    */
/************************************************************/

#include <fstream>
#include <iostream>
#include <iomanip>
#include "TermDblLinkList.h"
#include <chrono>

using namespace std;

/*********************************************************************
*   Function name:  TermDblLinkList() - constructor
*   Description:   	Allows for a TermDblLinkList to be created.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
  TermDblLinkList::TermDblLinkList(){}

/*********************************************************************
*   Function name:  readIntoList
*   Description:   	All data from file is extracted as a term and held in
*                   the specified container.
*   Parameters:  		filename - IMPORT - File used for stream extraction.
*
*   Return Value: 	N/A
*********************************************************************/
  void TermDblLinkList::readIntoList(string filename){
    ifstream source(filename.c_str());  //Opens the filestream
    double coeff;                       //Holds a coefficient
    int expn;                           //Holds an exponent
    bool found=false;                   //Flag for if the Term is found
    bool finalCycle=false;              //Flag for the final Terms

    while(source>>coeff>>expn){         //While the file is not empty,
      Term T(coeff,expn);               //Create a Term with extracted values
      DblLinkItr<Term> PolyItr(ThePoly);//Create an iterator for the list
      if(!PolyItr.isEmpty()){   //If the iterator is not empty,
        for(PolyItr.start();finalCycle==false&&found==false;++PolyItr){
          //Flag so the loop doesn't go past the last Node
          if(PolyItr.isLastNode()){
            finalCycle=true;
          }
          //If the current Term has the same exponent as the new one,
          if(PolyItr()==T){
              Term fused((T.getCoefficient()+ //Fuse the coefficients together
                PolyItr().getCoefficient()), T.getExponent());
              ThePoly.remove(PolyItr());  //Remove the old term
              ThePoly.insert(fused);      //Place the new one
              found=true;           //Flag that a duplicate exponent was found
          }
        }
      }
      if(found==false){   //If a duplicate was not found,
        ThePoly.insert(T);//Place it into the list
      }
      found=false;        //Reset the placement flag
      finalCycle=false;   //Reset the final Term flag
    }
    source.close();       //Close the filestream
    source.clear();       //Clear the filestream flag

    //The DblLinkList is sorted from the insert() commands.
  }

/*********************************************************************
*   Function name:  printIteratively
*   Description:   	All the Terms in the list are printed in polynomial form
*                   in iteration.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
  void TermDblLinkList::printIteratively(){
    using namespace std::chrono;  //Allows use of std::chrono commands.
    auto begin=high_resolution_clock::now();  //Finds start time.
    cout<<"---------------------------------"<<endl;
    cout<<"|DblLinkList  Iterative         |"<<endl;
    cout<<"---------------------------------"<<endl;
    DblLinkItr<Term> printer(ThePoly);  //Create a DblLink iterator
    if(!printer.isEmpty()){   //If the iterator is not empty,
      printer.start();        //Start the iterator
      while(!printer.isLastNode()){
        ++printer;            //Increment until it is the last node
      }
      //For all nodes that are not the last, print them and move to the previous
      for(;!printer.isFirstNode();--printer){
        cout<<printer()<<" + ";
      }
      cout<<printer()<<endl<<endl;  //Print the last node.
    }
    else{ //If the list is empty, let the user know.
      cout<<"There is nothing to be printed in the list."<<endl<<endl;
    }
    auto end=high_resolution_clock::now();  //Calculate end time for print,
    auto ticks=duration_cast<microseconds>(end-begin);  //Calculate time to print.
    cout<<"It took "<<ticks.count()<<" microseconds to print this."<<endl<<endl;
}

/*********************************************************************
*   Function name:  printRecursively
*   Description:   	All the Terms in the list are printed in polynomial form
*                   in recursion.
*   Parameters:  		count - IMPORT/EXPORT - holds the amount of times the
*                   recursion was executed.
*
*   Return Value: 	N/A
*********************************************************************/
  void TermDblLinkList::printRecursively(int count){
    int wasCount=count;   //Holds the count at the beginning of execution.
    using namespace std::chrono;  //Allows use of std::chrono commands.
    auto begin=high_resolution_clock::now();  //Finds start time.
    if(count==0){
      cout<<"---------------------------------"<<endl;
      cout<<"|DblLinkList  Recursive         |"<<endl;
      cout<<"---------------------------------"<<endl;
    }
    DblLinkItr<Term> printer(ThePoly);  //Create a DblLink iterator
    if(!printer.isEmpty()){   //If the iterator is not empty,
      printer.start();        //Start the iterator
      while(!printer.isLastNode()){
        ++printer;            //Increment until it is the last node
      }
      for(int i=0;i<count;i++){ //Decrement the interator to match the next for
        --printer;              //printing recursively.
      }
      if(!printer.isFirstNode()){ //If it is not the last node
        cout<<printer()<<" + ";   //Print it
        count++;                  //Increase count of recursion
        printRecursively(count);  //Start recursion
      }
      else{                       //For the last node,
        cout<<printer()<<endl<<endl;  //Print the last node.
      }
    }
    else{ //If the list is empty, let the user know.
      cout<<"There is nothing to be printed in the list."<<endl<<endl;
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
  double TermDblLinkList::operator()(double x) const
  { double result=0.0;
    DblLinkItr<Term> adder(ThePoly);  //Creates an iterator to add all terms
    if(!adder.isEmpty()){             //If the list is not empty,
      for(adder.start();!adder.isLastNode();++adder){ //For all Terms,
        result+=(adder())(x);         //Evaluate the Terms and add them
      }
      result+=(adder())(x);           //Evaluate the final Term
    }
    return(result);                   //Return the total
  }

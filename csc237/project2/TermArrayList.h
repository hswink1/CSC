/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: October 25, 2019 	   						              */
/* Due Date: October 30, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #2										                      	*/
/* Filename: TermArrayList.h  							           	    */
/* Purpose: Header for the TermArrayList class.             */
/************************************************************/

#ifndef WORDDATALIST_H
#define WORDDATALIST_H

#include <fstream>
#include <string>
#include "TermList.h"
#include "Term.h"

using namespace std;

class TermArrayList : public TermList {
public:

  //Constructor, set the numTerms to 0 as the list is empty.
  TermArrayList();

  /*Reads strings of text from a file and inserts it into a Term in the Array
  and sorts the Term in descending order.*/
  void readIntoList(string filename);

  //Print the data iteratively, by each index in polynomial form.
  void printIteratively();

  //Print the data recursively, by each index in polynomial form.
  void printRecursively(int count);

  //Print the data with a pointer to each index in polynomial form.
  void printPtr();

  /*Evaluate the Polynomial by inputting the value of 'x' for P(x) where P is
  polynomials to be evaluated.*/
  virtual double operator()(double x) const;

private:
  Term ThePoly[10]; //The Array that holds terms, set to a maximum of 10.
  int numTerms;     //The number of Terms in the Array.

};

#endif

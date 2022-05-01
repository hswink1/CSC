/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: October 25, 2019 	   						              */
/* Due Date: October 30, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #2										                      	*/
/* Filename: TermVectorList.h  							           	    */
/* Purpose: Header for the TermVectorList class.            */
/************************************************************/

#ifndef WORDDATAVECTORLIST_H
#define WORDDATAVECTORLIST_H

#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include "TermList.h"
#include "Term.h"

using namespace std;

class TermVectorList : public TermList {
public:

  //Constructor, allows the TermVectorList to be built.
  TermVectorList();

  /*Reads strings of text from a file and inserts it into a Term in the Array,
  then sorts the Terms in ascending order.*/
  void readIntoList(string filename);

  //Print the data iteratively and backwards, by each index in polynomial form.
  void printIteratively();

  //Print the data recursively and backwards, by each index in polynomial form
  void printRecursively(int count);

  /*Evaluate the Polynomial by inputting the value of 'x' for P(x) where P is
  polynomials to be evaluated.*/
  virtual double operator()(double x) const;

private:
  //The Vector which holds the Terms, changes size as Terms are inserted.
  vector<Term> ThePoly;

};

#endif

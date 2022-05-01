/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: October 25, 2019 	   						              */
/* Due Date: October 30, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #2										                      	*/
/* Filename: TermDblLinkList.h  							         	    */
/* Purpose: Header for the TermDblLinkList class.           */
/************************************************************/

#ifndef WORDDATALINKEDLIST_H
#define WORDDATALINKEDLIST_H

#include <fstream>
#include <string>
#include "Node.h"
#include "Term.h"
#include "TermList.h"

using namespace std;

class TermDblLinkList : public TermList {
public:

  //Constructor, allows the TermDblLinkList to be built.
  TermDblLinkList();

  /*Reads strings of text from a file and inserts it into a Term in the Array,
  then sorts the Terms in ascending order by an ordered insert.*/
  void readIntoList(string filename);

  //Print the data iteratively and backwards, by each index in polynomial form.
  void printIteratively();

  //Print the data recursively and backwards, by each index in polynomial form
  void printRecursively(int count);

  /*Evaluate the Polynomial by inputting the value of 'x' for P(x) where P is
  polynomials to be evaluated.*/
  virtual double operator()(double x) const;

private:
  /*The DblLinkList container which holds the Terms with pointers to the
  next and previous Terms.*/
  DblLink<Term> ThePoly;

};

#endif

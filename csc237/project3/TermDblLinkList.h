/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 10, 2019 	   		  		              */
/* Due Date: November 13, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #3										                      	*/
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

/**
* \file TermDblLinkList.h
* \class TermDblLinkList
* \brief This class reads from file and holds Terms in an DblLink container to
*        be printed and evaluated as polynomials.
**/

class TermDblLinkList : public TermList {
public:

  /**
  * \brief Allows for a TermDblLinkList to be created.
  * \param N/A
  * \return N/A
  **/
  //Constructor, allows the TermDblLinkList to be built.
  TermDblLinkList();

  /**
  * \brief All data from file is extracted as a term and held in
  *        the specified container.
  * \param filename - IMPORT - File used for stream extraction.
  * \return	N/A
  **/
  /*Reads strings of text from a file and inserts it into a Term in the Array,
  then sorts the Terms in ascending order by an ordered insert.*/
  void readIntoList(string filename);

  /**
  * \brief All the Terms in the list are printed in polynomial form
  *        in iteration.
  * \param N/A
  * \return N/A
  **/
  //Print the data iteratively and backwards, by each index in polynomial form.
  void printIteratively();

  /**
  * \brief All the Terms in the list are printed in polynomial form
  *        in recursion.
  * \param count - IMPORT/EXPORT - holds the amount of times the
  *        recursion was executed.
  * \return N/A
  **/
  //Print the data recursively and backwards, by each index in polynomial form
  void printRecursively(int count);

  /**
  * \brief All the Terms in the list are evaluated and totaled
  *        with the user input for 'x'.
  * \param N/A
  * \return double - the total of all the Terms
  **/
  /*Evaluate the Polynomial by inputting the value of 'x' for P(x) where P is
  polynomials to be evaluated.*/
  virtual double operator()(double x) const;

private:
  /*The DblLinkList container which holds the Terms with pointers to the
  next and previous Terms.*/
  DblLink<Term> ThePoly;

};

#endif

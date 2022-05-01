/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 10, 2019 	   		  		              */
/* Due Date: November 13, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #3										                      	*/
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

/**
* \file TermVectorList.h
* \class TermVectorList
* \brief This class reads from file and holds Terms in an Vector container to be
*        printed and evaluated as polynomials.
**/

class TermVectorList : public TermList {
public:

  /**
  * \brief Allows for a TermVectorList to be created.
  * \param N/A
  * \return N/A
  **/
  //Constructor, allows the TermVectorList to be built.
  TermVectorList();

  /**
  * \brief All data from file is extracted as a term and held in
  *        the specified container.
  * \param filename - IMPORT - File used for stream extraction.
  * \return N/A
  **/
  /*Reads strings of text from a file and inserts it into a Term in the Array,
  then sorts the Terms in ascending order.*/
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
  *        through recursion.
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
  //The Vector which holds the Terms, changes size as Terms are inserted.
  vector<Term> ThePoly;

};

#endif

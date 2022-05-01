/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 10, 2019 	   		  		              */
/* Due Date: November 13, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #3										                      	*/
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

/**
* \file TermArrayList.h
* \class TermArrayList
* \brief This class reads from file and holds Terms in an Array container to be
* printed and evaluated as polynomials.
**/

class TermArrayList : public TermList {
public:

  /**
  * \brief Allows for a TermArrayList to be created.
  * \param N/A
  * \return N/A
  **/
  //Constructor, set the numTerms to 0 as the list is empty.
  TermArrayList();

  /**
  * \brief All data from file is extracted as a term and held in
  *        the specified container.
  * \param filename - IMPORT - File used for stream extraction.
  * \return N/A
  **/
  /*Reads strings of text from a file and inserts it into a Term in the Array
  and sorts the Term in descending order.*/
  void readIntoList(string filename);

  /**
  * \brief All the Terms in the list are printed in polynomial form
  *        in iteration.
  * \param N/A
  * \return N/A
  **/
  //Print the data iteratively, by each index in polynomial form.
  void printIteratively();

  /**
  * \brief All the Terms in the list are printed in polynomial form
  *        through recursion.
  * \param count - IMPORT/EXPORT - holds the amount of times the
  *        recursion was executed.
  * \return N/A
  **/
  //Print the data recursively, by each index in polynomial form.
  void printRecursively(int count);

  /**
  * \brief All the Terms in the list are printed in polynomial form
  *        by a pointer.
  * \param N/A
  * \return N/A
  **/
  //Print the data with a pointer to each index in polynomial form.
  void printPtr();

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
  Term ThePoly[10]; //The Array that holds terms, set to a maximum of 10.
  int numTerms;     //The number of Terms in the Array.

};

#endif

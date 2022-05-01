/***************************************************************
Author:         Dr. Spiegel
Filename:       Term.h
Purpose:        A Term holds one term of a polynomial. A term
		can be created, evaluated, compared, and printed
***************************************************************/
#ifndef TERM_H
#define TERM_H

#include <iostream>

using namespace std;

/**
* \file Term.h
* \class Term
* \brief A Term holds a polynomial as a double for the coefficient and an
*				 integer for the exponent, conceptualized in the form 0x^0.
**/

class Term
{
public:
	//Constructor
	Term(double=0, int=0); 	//default value of 0x^0

	/**
	* \brief Retrieves the coefficient
	* \param n/a
	* \return double- the coefficient at the index
	**/
	//Gets (No sets in immutable object)
	double getCoefficient() const; 		//returns the coefficient

	/**
	* \brief retrieves the exponent
	* \param n/a
	* \return int- the exponent at the index
	**/
	int getExponent() const; 		//returns the exponent

	/**
	* \brief Evaluates the term by the input number
	* \param double x- the number to evaluate to
	* \return the sum of the evaluated term
	**/
	//evaluate
	double operator()(double x) const; 	// evaluation of term

	//other
	/**
	* \brief Compares an int against the exponent
	* \param int value- the number to compare the exponent by
	* \return true- if match // false- if doesn't match
	**/
	// does exponent match the parameter? Note signature ==(Term&,int)
	bool operator==(int value);

	/**
	* \brief Compares a Term's exponent against the exponent
	* \param Term- the Term to compare the exponent by
	* \return true- if match // false- if doesn't match
	**/
	// does exponent match that of the param?  Signature: ==(Term,Term)
	bool operator==(const Term &right);

	/**
	* \brief Compares a Term's exponent against the exponent
	* \param Term right- the Term to compare the exponent by
	* \return true- if not match // false- if match
	**/
	bool operator!=(const Term &right);

	/**
	* \brief Fuses the coefficients of two terms, assumed to be
	*				 of the same exponent.
	* \param Term - the Term to add
	* \return The fusion of the two terms
	**/
	Term& operator+=(const Term &right);

	/**
	* \brief Compares two Terms based on exponent
	* \param int value- the value to make the comparision to
	* \return according to the comparison
	**/
	// is this term's exponent less than right's? Signature <(Term&,Term&)
	bool operator<(const Term &right)const;

	/**
	* \brief Compares Term's exponent to an int
	* \param int value- the value to make the comparision to
	* \return according to the comparison
	**/
	// is this term's exponent less than right? Signature: <(Term&,int)
	bool operator<(int right);

private:
	double coefficient; //holds the coefficient value
	int exponent; //holds the exponent value

};
/**
* \brief For output
* \param ofstream &ouput-
*	\param const Term &t- the term object to be outputted
* \return output
**/
	// Stream insert a term
	ostream &operator<<(ostream &output,const Term &term);
#endif

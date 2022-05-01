/******************************************************************

     Author:        Henry Winkleman
     Major:         Philosophy
     Creation Date: 3/24/18
     Due Date:      3/26/18   
     Course:        CSC135-020 
     Professor:     Dr. Joo Tan 
     Assignment:    Project 2
     Filename:      proj2_Winkleman.cpp
     Purpose:       A program used to calculate bookstore purchases.

******************************************************************/

#include <iostream>
#include <iomanip>
using namespace std;
//Prototypes
void welcome();
int cartsize();
float calccart(int cart);
float discount(float subtotal);
float totalcart(float subtotal, float discount);
void showcost(int cart, float subtotal, float discount, float total);

int main() {
	welcome(); //Introduction to the bookstore.
	int cart =cartsize(); //User input for the books they are purchasing.
	float subtotal =calccart(cart); //Finds subtotal for the books chosen.
	float cartdiscount =discount(subtotal); //Finds 15% discount on the subtotal of the books.
	float total =totalcart(subtotal, cartdiscount); //Calculates total for the books with the discount.
	showcost(cart, subtotal, cartdiscount, total); //Prints the books chosen, subtotal, discount, then total.
return 0; }

//Functions
void welcome() {
	cout<< endl << "-----------------------------------------"<< endl << "Welcome to BandN!" << endl;
	cout<<"Every book today is $8.99 and an extra 15% discount!" << endl << "-----------------------------------------" <<endl<<endl;}
/*************************************************************************

    Function name: 	welcome
    Description: 	Prints welcome text for the bookstore.
    Parameters: 	None.
    Return Value: 	None.
 
*************************************************************************/

int cartsize(){
	int book;
	cout << "How many books are you purchasing: ";
	cin >> book;
return book;}
/*************************************************************************

    Function name: 	cartsize
    Description: 	User input for total books chosen.
    Parameters: 	None.
    Return Value: 	cart - Books chosen.
 
*************************************************************************/

float calccart(int cart){
	float total =cart*8.99;
return total;}
/*************************************************************************

    Function name: 	calccart
    Description: 	Calculates the raw cost of books.
    Parameters: 	int cart: Number of books - input
    Return Value: 	subtotal - Cost of the books without discount.
 
*************************************************************************/

float discount(float subtotal){
	float discount =0.15;
	float discountsubtotal =subtotal*discount;
return discountsubtotal;}
/*************************************************************************

    Function name: 	discount 
    Description: 	Calculates the discount for the cart
    Parameters: 	float subtotal: Cost of books without discount - input 
    Return Value: 	cartdiscount - Money saved by discount.
 
*************************************************************************/

float totalcart(float subtotal, float discount){
	float total =subtotal-discount;
return total;}
/*************************************************************************

    Function name: 	totalcart 
    Description: 	Calculate the total for the books with the discount.
    Parameters: 	float subtotal: Cost of books without discount - input 
					float discount: Amount saved by discount – input
    Return Value: 	total - The total in the cart.
 
*************************************************************************/

void showcost(int cart, float subtotal, float discount, float total){
	cout<<endl;
	cout<<"================|Cart|================"<<endl;
	cout<<"Books in cart:"<<setw(23)<<cart<<endl;
	cout<<"Cart subtotal:"<<setw(16)<<"$"<<setw(8)<<setprecision(2)<<fixed<<subtotal<<endl;
	cout<<"Cart with discount applied:"<<setw(3)<<"$"<<setw(8)<<setprecision(2)<<fixed<<discount<<endl;
cout<<"Cart total:"<<setw(19)<<"$"<<setw(8)<<setprecision(2)<<fixed<<total<<endl<<endl;}
/*************************************************************************

    Function name: 	showcost
    Description: 	Prints the amount of books, subtotal, discount, and total.
    Parameters: 	int cart: Number of books - input 
					float subtotal: Cost of books without discount - input 
					float discount: Amount saved by discount – input
					float total: Total cost in cart – input					
    Return Value: 	None. 
 
*************************************************************************/
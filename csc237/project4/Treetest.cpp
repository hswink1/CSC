/************************************************************/
/* Contributor: Henry Winkleman                             */
/* Edit Date: November 26, 2019 	   		  		              */
/* Due Date: November 28, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #4										                      	*/
/* Filename: Treetest.h         						           	    */
/* Purpose: Uses the Binary Tree with Terms in order to more*/
/* easily sort and find Terms in the tree.                  */
/************************************************************/

#include <iostream>
#include <string>
#include "Term.h"
#include "BinarySearchTree.h"

using namespace std;

/**
* \file Treetest.cpp
* \brief Uses the Binary Tree with Terms in order to more easily sort and find
* Terms in the tree.
**/

typedef BinaryTree<Term> BinTree;

/**
* \brief Converts lowercase to uppercase valid inputs for choice.
* \param string ok - IMPORT - The conversion for characters to be used for the choice.
* \return char - the converted choice
**/
char getChoice(string ok);

/**
* \brief User inputs a value for the new Term and it is added to the tree.
* \param BinTree Tree - IMPORT/EXPORT - the tree to be have the value added to.
* \return N/A
**/
void addingToTree(BinTree&);

/**
* \brief User inputs a value to be removed from the tree.
* \param BinTree Tree - IMPORT/EXPORT - the tree to be have the value removed from.
* \return N/A
**/
void removingFromTree(BinTree&);

/**
* \brief User inputs an exponent for a Term to be changed to.
* \param BinTree Tree - IMPORT/EXPORT - the tree to be have the value changed from.
* \return N/A
**/
void changeNode(BinTree&);

int main()
{
 BinTree Tree;
 int result;
 int expn=0;
 char Choice;
 do { //First run.
  cout<<"Select: A)dd    R)emove    C)hange     P)rint     T)ree Print     Q)uit\n";
  Choice=getChoice("ARCPTQ"); //Looks for only value choices.
  switch (Choice) { //Changes program function based on choice.
   case 'A':        //Add value
    addingToTree(Tree);
    break;
   case 'R':        //Remove value
    removingFromTree(Tree);
    break;
   case 'C':        //Change value
    changeNode(Tree);
    break;
   case 'P':        //Print in descending (by exponent) order
    cout << "The Tree:" << endl;
    Tree.revOrder('+', true);
    break;
   case 'T':        //Print in drawn format.
    cout << "The tree, as it appears (sort of)..\n";
    Tree.treePrint();
  }
} while (Choice!='Q'); //While user has not ended program.
}

/*********************************************************************
*   Function name:  getChoice
*   Description:  Converts lowercase to uppercase valid inputs for choice.
*   Parameters: string ok - IMPORT - The conversion for characters to
*               be used for the choice.
*
*   Return Value: char - the converted choice
*********************************************************************/
char getChoice(string ok)
{char ch=' ';
 do ch=toupper(cin.get()); while (ok.find(ch)==string::npos);
 cin.get(); // eat CR
 return(toupper(ch));
}

/*********************************************************************
*   Function name:  addingToTree
*   Description:  User inputs a value for the new Term and it is added
*                 to the tree.
*   Parameters: BinTree Tree - IMPORT/EXPORT - the tree to be have the
*               value added to.
*
*   Return Value: N/A
*********************************************************************/
void addingToTree(BinTree& Tree){
  double coeff=0;
  int expn=0;
  int result;
  cout << " Enter an Integer for the Coefficient >";
  cin >> coeff;
  cout << " Enter an Integer for the Exponent >";
  cin >> expn;
  Term newPoly(coeff, expn);  //Creates a new term to be added
  result = Tree.insertToTree(newPoly);  //Adds the term, returns if new
  if(result==0){  //If not new
    Term checkTerm=Tree.retrieveFromTree(newPoly);  //Find where it was added
    if(checkTerm.getCoefficient()==0){  //If the coeff is 0
      Tree.deleteFromTree(newPoly);     //Delete the Term
    }
  }
}

/*********************************************************************
*   Function name:  removingFromTree
*   Description:  User inputs a value to be removed from the tree.
*   Parameters: BinTree Tree - IMPORT/EXPORT - the tree to be have the
*               value removed from.
*
*   Return Value: N/A
*********************************************************************/
void removingFromTree(BinTree& Tree){
  double coeff=0;
  int expn=0;
  int result;
  cout << "Value to Delete (by Exponent)? >";
  cin >> expn;
  Term newPoly(coeff, expn);        //Creates a term to be searched for.
  result=Tree.treeSearch(newPoly);  //Searches for the term
  if (!result) cout << "Polynomial of " << expn << " Not Found\n";
  else Tree.deleteFromTree(newPoly);//Poly was found, deletes it.
}

/*********************************************************************
*   Function name:  changeNode
*   Description:  User inputs an exponent for a Term to be changed to.
*   Parameters: BinTree Tree - IMPORT/EXPORT - the tree to be have the
*               value changed from.
*
*   Return Value: N/A
*********************************************************************/
void changeNode(BinTree& Tree){
  int newExpn, searchExpn, result;
  cout<<" Enter the Term's Exponent to Change >";
  cin>>searchExpn;
  cout<<" Enter the Exponent It Will be Changed to >";
  cin>>newExpn;
  Term dummy(1, searchExpn);  //Creates node to be searched for.
  if(Tree.treeSearch(dummy)){ //Searches for node, if found
    Term oldTerm=Tree.retrieveFromTree(dummy);      //Creates copy of the old node
    //Copies over the coeff from old node to the new node
    Term newTerm(oldTerm.getCoefficient(), newExpn);
    result=Tree.change(oldTerm, newTerm); //Changes the Term in the node to be changed
    if(result==0){                        //If the new Term shares an exponent
      Term checkTerm=Tree.retrieveFromTree(newTerm);  //Find the fused Term
      if(checkTerm.getCoefficient()==0){              //If the coeff is 0
        Tree.deleteFromTree(newTerm);                 //Remove the Term
      }
    }
  }
}

/************************************************************/
/* Contributor: Henry Winkleman 							              */
/* Edit Date: October 7, 2019 	   						              */
/* Due Date: October 18, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #2										                      	*/
/* Filename: testLL.cpp 							                     	*/
/* Purpose: This program creates a list of integers that    */
/*  users can add or remove from, along with printing those */
/*  ordered numbers in an ascending or descending order.    */
/************************************************************/

#include <iostream>
#include "Node.h"

using namespace std;

//Prototypes for functions
//Universal
int selection();

//Situational
void insertProcess(DblLink<int>&);
void removalProcess(DblLink<int>&);
void printForward(const DblLink<int>&);
void printBackward(const DblLink<int>&);

int main(){
  //Creates the doubly-linked list.
  DblLink<int> testLink;

  //Loop will perform until exit is chosen.
  while(true){
    //User chooses task to be performed.
    int option=selection();
    switch(option){
      //Insert
      case 1: insertProcess(testLink);
              break;
      //Remove
      case 2: removalProcess(testLink);
              break;
      //Print forwards
      case 3: printForward(testLink);
              break;
      //Print backwards
      case 4: printBackward(testLink);
              break;
      //Exit
      case 5: cout<<"Exiting the program."<<endl<<endl;
              return 0;
              break;
    }
  }
  return 0;
}

/*********************************************************************
*   Function name:  selection
*   Description:   	User chooses one of five tasks to be performed.
*   Parameters:  		N/A
*
*   Return Value: 	int - option chosen.
*********************************************************************/
int selection(){
  int choiceMade=0;

  cout<<"*******************************************************"<<endl;
  cout<<"Please choose a task to be performed."<<endl;
  cout<<"1. Insert a number into the list."<<endl;
  cout<<"2. Remove a number from the list."<<endl;
  cout<<"3. Print all numbers in ascending (forward) order."<<endl;
  cout<<"4. Print all numbers in descending (backward) order."<<endl;
  cout<<"5. Quit the program."<<endl;
  cout<<"*******************************************************"<<endl;
  cout<<"User choice: ";
  cin>>choiceMade;  //User makes a choice
  cout<<endl;
  while(choiceMade<1||choiceMade>5){  //If the choice is not valid,
    cin.clear();
    cin.ignore(10000, '\n');
    cout<<"Invalid choice, please enter a valid choice: ";
    choiceMade=0;
    cin>>choiceMade;                  //They re-enter a choice.
    cout<<endl;
  }
  return choiceMade;
}

/*********************************************************************
*   Function name:  insertProcess
*   Description:   	Inserts a chosen number to the list.
*   Parameters:  	  testList - The list to be inserted into.
*
*   Return Value: 	N/A
*********************************************************************/
void insertProcess(DblLink<int>& testList){
  int chosenNumber;

  cout<<"Choose a number to be entered: ";
  cin>>chosenNumber;              //User enters the number.
  cout<<endl;

  testList.insert(chosenNumber);  //Inserts the number.
}

/*********************************************************************
*   Function name:  removalProcess
*   Description:   	Removes a chosen number to the list.
*   Parameters:  	  testList - The list to be removed from.
*
*   Return Value: 	N/A
*********************************************************************/
void removalProcess(DblLink<int>& testList){
  int chosenNumber;
  bool success=true;
  cout<<"Choose a number to be removed: ";
  cin>>chosenNumber;              //User enters the number.
  cout<<endl<<endl;

  //Removes the first node of that number.
  success=testList.remove(chosenNumber);
  if(!success){
    cout<<"The number was not found in the list."<<endl<<endl;
  }
}

/*********************************************************************
*   Function name:  printForward
*   Description:   	Prints the list from start to finish.
*   Parameters:  		testLink - IMPORT - The list to be printed.
*
*   Return Value: 	N/A
*********************************************************************/
void printForward(const DblLink<int>& testLink){
  DblLinkItr<int> testItr(testLink);
  if(!testItr.isEmpty()){   //If the iterator is not empty,
    //For all nodes that are not the last, print them and move to the next.
    for(testItr.start();!testItr.isLastNode();testItr++){
      cout<<testItr()<<endl;
    }
    cout<<testItr()<<endl<<endl;  //Print the last node.
  }
  else{ //If the list is empty, let the user know.
    cout<<"There is nothing to be printed in the list."<<endl<<endl;
  }
}

/*********************************************************************
*   Function name:  printBackward
*   Description:   	Prints from the end to the beginning.
*   Parameters:  		testCopy - IMPORT - The list to be printed.
*
*   Return Value: 	N/A
*********************************************************************/
void printBackward(const DblLink<int>& testLink){
  DblLinkItr<int> testItr(testLink);
  if(!testItr.isEmpty()){ //If the list is not empty,
    //Move to the end of the list.
    for(testItr.start();!testItr.isLastNode();testItr++){}
    for(;!testItr.isFirstNode();testItr--){ //Print all of the values and
        cout<<testItr()<<endl;              //head towards the beginning.
    }
    cout<<testItr()<<endl<<endl;            //Print the first node.
  }
  else{ //If the list is empty, let the user know.
    cout<<"There is nothing to be printed in the list."<<endl<<endl;
  }
}

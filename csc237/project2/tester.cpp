
#include <iostream>
#include "Node.h"

using namespace std;

int selection();
void insertProcess(DblLink<int>&);
void removalProcess(DblLink<int>&);
void printForward(const DblLink<int>&);

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
      case 4:
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
  cin>>choiceMade;
  cout<<endl;
  while(choiceMade<1||choiceMade>5){
    cin.clear();
    cin.ignore(10000, '\n');
    cout<<"Invalid choice, please enter a valid choice: ";
    choiceMade=0;
    cin>>choiceMade;
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

  cout<<"Choose a number to be removed: ";
  cin>>chosenNumber;              //User enters the number.
  cout<<endl<<endl;

  testList.remove(chosenNumber);  //Removes the first node of that number.
}

void printForward(const DblLink<int>& testLink){
  DblLinkItr<int> testItr(testLink);
  if(!testItr.isEmpty()){
    for(testItr.start();!testItr.isLastNode();testItr++){
      cout<<testItr()<<endl;
    }
    cout<<testItr()<<endl;
  }
  else{
    cout<<"There is nothing to be printed in the list."<<endl<<endl;
  }
}

/************************************************************/
/* Contributor: Henry Winkleman 							              */
/* Edit Date: October 25, 2019 	   						              */
/* Due Date: October 30, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #2										                      	*/
/* Filename: app.cpp 							                     	    */
/* Purpose: This program reads a file into polynomials that */
/* are held in user-selected containers, to then be printed */
/* or evaluated by an user input for 'x'.                   */
/************************************************************/

#include <iostream>
#include "Term.h"
#include "TermList.h"
#include "TermArrayList.h"
#include "TermVectorList.h"
#include "TermDblLinkList.h"

using namespace std;

//Prototypes for functions
//Universal
void batchTest(const string&, const double&);
bool fileEntry(ifstream&, string&);
int selection();
double evalSelect(const TermList*);

//Situational
int TArrayLOption(const string&);
  int TArrayLSelect();
int TVectorLOption(const string&);
  int TVectorLSelect();
int TDblLinkLOption(const string&);
  int TDblLinkLSelect();

int main(int argc, char **argv){  //Sets a condition for batch testing
  ifstream ifs;   //Used for filestream
  string filename;//Holds the file-to-be-opened's name
  bool exit;      //Flag to signal file input failure

  if(argc==1){                    //If the program is launched normally,
    exit=fileEntry(ifs, filename);//User enters the filename
    if(!exit){                    //If there is a failure, the program closes.
      return 0;
    }
    while(true){  //Loop performs until canceled.
      int option=selection(); //User chooses the Term container to be used
      switch(option){
        case 1: TArrayLOption(filename);  //Options open for the TermArrayList
                break;
        case 2: TVectorLOption(filename); //Options open for the TermVectorList
                break;
        case 3: TDblLinkLOption(filename);//Options open for the TermDblLinkList
                break;
        case 4: cout<<"Exiting the program."<<endl<<endl;
                return 0;
                break;
      }
    }
  }
  //BATCHMODE
  else if(argc==3){   //If the program is launched with two other inputs,
    filename=argv[1]; //The first input is the filename
    double chosenNumber=atof(argv[2]);  //The second input is 'x' for evaluation
    batchTest(filename, chosenNumber);  //Batchmode launches
    return 0;
  }
  else{ //Closes on any other argc number
    return 0;
  }
}

/*********************************************************************
*   Function name:  batchTest
*   Description:    Launches all outputs and evaluations for every class.
*   Parameters:  	  filename - IMPORT - the file to be read.
*                   chosenNumber - IMPORT - the number used in evaluation.
*
*   Return Value: 	N/A
*********************************************************************/
void batchTest(const string& filename, const double& chosenNumber){
  TermList* ThePolyA=new TermArrayList; //Creates a TermArrayList
  ThePolyA->readIntoList(filename);     //Reads the file into it
  ThePolyA->printIteratively();         //Prints the list iteratively
  cout<<"P(x) = "<<(*ThePolyA)(chosenNumber)<<endl; //Evaluates the polynomials
  ThePolyA->printPtr();                 //Prints the list with a pointer
  cout<<"P(x) = "<<(*ThePolyA)(chosenNumber)<<endl; //Evaluates the polynomials
  ThePolyA->printRecursively();         //Prints the list recursively
  cout<<"P(x) = "<<(*ThePolyA)(chosenNumber)<<endl; //Evaluates the polynomials

  TermList* ThePolyV=new TermVectorList;//Creates a TermVectorList
  ThePolyV->readIntoList(filename);   //Reads the file into it.
  ThePolyV->printIteratively();       //Prints the list iteratively
  cout<<"P(x) = "<<(*ThePolyV)(chosenNumber)<<endl; //Evaluates the polynomials
  ThePolyV->printRecursively();       //Prints the list recursively
  cout<<"P(x) = "<<(*ThePolyV)(chosenNumber)<<endl; //Evaluates the polynomials

  TermList* ThePolyDL=new TermDblLinkList;//Creates a TermDblLinkList
  ThePolyDL->readIntoList(filename);  //Reads the file into it.
  ThePolyDL->printIteratively();      //Prints the list iteratively
  cout<<"P(x) = "<<(*ThePolyDL)(chosenNumber)<<endl; //Evaluates the polynomials
  ThePolyDL->printRecursively();      //Prints the list recursively
  cout<<"P(x) = "<<(*ThePolyDL)(chosenNumber)<<endl; //Evaluates the polynomials
  cout<<endl;
}

/*********************************************************************
*   Function name:  fileEntry
*   Description:   	User enters the file to be read.
*   Parameters:  		ifs - IMPORT/EXPORT - the input stream.
*
*   Return Value: 	bool - returns stream opening success.
*********************************************************************/
bool fileEntry(ifstream& ifs, string& filename){
  string testEmpty; //Checks for ifs.eof()

  cout<<"Please enter the file to be read: ";
  cin>>filename;              //User enters filename.
  ifs.open(filename.c_str()); //Stream is opened.
  if(!ifs){                   //Checks for success.
      cout<<"Failure to open the file. Closing program."<<endl;
      return false;           //If failure, the program closes.
  }
  ifs>>testEmpty; //Tests to see if there is content in the file
  if(ifs.eof()){  //If there is no content,
    cout<<"The file is empty. Closing program."<<endl;
    return false; //Close the program
  }
  ifs.close();  //Closes the filestream
  ifs.clear();  //Clears the filestream eof flag
  return true;
}

/*********************************************************************
*   Function name:  selection
*   Description:   	User chooses one of four tasks to be performed.
*   Parameters:  		N/A
*
*   Return Value: 	int - option chosen.
*********************************************************************/
int selection(){
  int choiceMade=0;

  cout<<"*******************************************************"<<endl;
  cout<<"Please choose a task to be performed."<<endl;
  cout<<"1. Term Array prints."<<endl;
  cout<<"2. Term Vector print."<<endl;
  cout<<"3. Term DblLink print."<<endl;
  cout<<"4. Quit the program."<<endl;
  cout<<"*******************************************************"<<endl;
  cout<<"User choice: ";
  cin>>choiceMade;  //User makes a choice
  cout<<endl;
  while(choiceMade<1||choiceMade>4){  //If the choice is not valid,
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
*   Function name:  evalSelect
*   Description:   	Universal function for evaluation of P(x).
*   Parameters:  		ThePoly - IMPORT - the pointer to the container which holds
*                   the Terms.
*
*   Return Value: 	double - the value of P(x).
*********************************************************************/
double evalSelect(const TermList* ThePoly){
  double chosenNumber;

  cout<<"Choose a value for 'x': ";
  cin>>chosenNumber;    //User chooses a number for 'x'
  cout<<endl;
  //The Terms are evaluated by a class specific overloaded operator
  cout<<"P(x) = "<<(*ThePoly)(chosenNumber)<<endl;
  return chosenNumber;
}

/*********************************************************************
*   Function name:  TArrayLOption
*   Description:   	Holds the function routine for the TermArrayList options.
*   Parameters:  		filename - IMPORT - the file to be read.
*
*   Return Value: 	int - used to return 0 to break the infinite loop.
*********************************************************************/
int TArrayLOption(const string& filename){
  int choice;                           //Holds the user choice
  TermList* ThePoly=new TermArrayList;  //A TermArrayList is made

  ThePoly->readIntoList(filename);    //The file is read into the TermArrayList
  while(true){  //Loop works until cancelled
    choice=TArrayLSelect(); //The user makes a choice from offered functions
    switch(choice){
      case 1: ThePoly->printIteratively();  //The list is printed iteratively
              evalSelect(ThePoly);          //The Terms are evaluated
              break;
      case 2: ThePoly->printPtr();          //The list is printed by a pointer
              evalSelect(ThePoly);          //The Terms are evaluated
              break;
      case 3: evalSelect(ThePoly);          //The Terms are evaluated
              break;
      case 4: return 0;                     //Exits to the menu in main()
              break;
    }
  }
}

/*********************************************************************
*   Function name:  TArrayLSelect
*   Description:   	User chooses the function to be performed from TArrayLOption
*   Parameters:  		N/A
*
*   Return Value: 	int - the choice made.
*********************************************************************/
int TArrayLSelect(){
  int choiceMade=0;
  cout<<"*******************************************************"<<endl;
  cout<<"Choose a function for the TermArrayList class,"<<endl;
  cout<<"1. Display all the terms in an iterative fashion."<<endl;
  cout<<"2. Display all the terms with a pointer."<<endl;
  cout<<"3. Evaluate the polynomials (P(x))."<<endl;
  cout<<"4. Choose another data structure."<<endl;
  cout<<"*******************************************************"<<endl;
  cout<<"Selection: ";
  cin>>choiceMade;                    //User chooses a function to be performed
  while(choiceMade>4||choiceMade<1){  //If it is not a valid function,
    cin.clear();
    cin.ignore(10000, '\n');
    choiceMade=0;
    cout<<"Choose a valid option: ";
    cin>>choiceMade;                  //User re-enters an option
  }
  cout<<endl;
  return choiceMade;                  //Choice is returned
}

/*********************************************************************
*   Function name:  TVectorLOption
*   Description:   	Holds the function routine for the TermVectorList options.
*   Parameters:  		filename - IMPORT - the file to be read.
*
*   Return Value: 	int - used to return 0 to break the infinite loop.
*********************************************************************/
int TVectorLOption(const string& filename){
  int choice;                           //Holds the user choice
  TermList* ThePoly=new TermVectorList; //A TermVectorList is made

  ThePoly->readIntoList(filename);  //The file is read into the TermVectorList
  while(true){  //Loop works until cancelled
    choice=TVectorLSelect();  //The user makes a choice from offered functions
    switch(choice){
      case 1: ThePoly->printIteratively();  //The list is printed by an iterator
              evalSelect(ThePoly);          //The Terms are evaluated
              break;
      case 2: evalSelect(ThePoly);          //The Terms are evaluated
              break;
      case 3: return 0;                     //Exits to the menu in main()
              break;
    }
  }
}

/*********************************************************************
*   Function name:  TVectorLSelect
*   Description:   	User chooses the function to be performed from TVectorLOption
*   Parameters:  		N/A
*
*   Return Value: 	int - the choice made.
*********************************************************************/
int TVectorLSelect(){
  int choiceMade=0;
  cout<<"*******************************************************"<<endl;
  cout<<"Choose a function for the TermVectorList class,"<<endl;
  cout<<"1. Display all the terms in an iterative fashion."<<endl;
  cout<<"2. Evaluate the polynomials (P(x))."<<endl;
  cout<<"3. Choose another data structure."<<endl;
  cout<<"*******************************************************"<<endl;
  cout<<"Selection: ";
  cin>>choiceMade;                    //User chooses a function to be performed
  while(choiceMade>3||choiceMade<1){  //If it is not a valid function,
    cin.clear();
    cin.ignore(10000, '\n');
    choiceMade=0;
    cout<<"Choose a valid option: ";
    cin>>choiceMade;                  //User re-enters an option
  }
  cout<<endl;
  return choiceMade;                  //Choice is returned
}

/*********************************************************************
*   Function name:  TDblLinkLOption
*   Description:   	Holds the function routine for the TermDblLinkList options.
*   Parameters:  		filename - IMPORT - the file to be read.
*
*   Return Value: 	int - used to return 0 to break the infinite loop.
*********************************************************************/
int TDblLinkLOption(const string& filename){
  int choice;                             //Holds the user choice
  TermList *ThePoly=new TermDblLinkList;  //A TermDblLinkList is made

  ThePoly->readIntoList(filename);  //The file is read into the TermDblLinkList
  while(true){  //Loop works until cancelled
    choice=TDblLinkLSelect(); //The user makes a choice from offered functions
    switch(choice){
      case 1: ThePoly->printIteratively();  //The list is printed by an iterator
              evalSelect(ThePoly);          //The Terms are evaluated
              break;
      case 2: evalSelect(ThePoly);          //The Terms are evaluated
              break;
      case 3: return 0;                     //Exits to the menu in main()
              break;
    }
  }
}

/*********************************************************************
*   Function name:  TDblLinkLSelect
*   Description:   	User chooses the function to be performed from TDblLinkLOption
*   Parameters:  		N/A
*
*   Return Value: 	int - the choice made.
*********************************************************************/
int TDblLinkLSelect(){
  int choiceMade=0;
  cout<<"*******************************************************"<<endl;
  cout<<"Choose a function for the TermDblLinkList class,"<<endl;
  cout<<"1. Display all the terms in an iterative fashion."<<endl;
  cout<<"2. Evaluate the polynomials (P(x))."<<endl;
  cout<<"3. Choose another data structure."<<endl;
  cout<<"*******************************************************"<<endl;
  cout<<"Selection: ";
  cin>>choiceMade;                    //User chooses a function to be performed
  while(choiceMade>3||choiceMade<1){  //If it is not a valid function,
    cin.clear();
    cin.ignore(10000, '\n');
    choiceMade=0;
    cout<<"Choose a valid option: ";
    cin>>choiceMade;                  //User re-enters an option
  }
  cout<<endl;
  return choiceMade;                  //Choice is returned
}

/************************************************************/
/* Contributor: Henry Winkleman 							*/
/* Edit Date: December 3, 2018 								*/
/* Due Date: December 5, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #4											*/
/* Filename: p4.cpp 										*/
/* Purpose: Tests LinkedList with a WordRec subtype,		*/
/* 			reads words in a file and provides a variety	*/
/*			of display and edit options.					*/
/************************************************************/
//Prepared by Dr. Spiegel with thanks to Jamie Mason

#include <iostream>
#include <string>
#include <fstream>
#include <iomanip>
#include "LinkedList.h"
#include "WordRec.h"

using namespace std;

typedef LinkedList<WordRec> WordList;
bool openFile(ifstream &inf);
char menu();
int numberPrompt();
string stringPrompt();
void find(const WordList &TheWords);
void substring(const WordList &TheWords);
void mult(const WordList &TheWords);
void multMchars(const WordList &TheWords);
void removeWord(WordList &TheWords);
void regulateOutput(WordList &TheWords);

ifstream &operator>>(ifstream &inf, WordList &right);

int main(){
	
  ifstream inf;	//Defines the input filestream
  char character;	//Declares a variable of character type
  bool result;	//Used for file open result
  LinkedList<WordRec> TheWords;	//Creates a wordlist
  
  result=openFile(inf);	//Determines if file was opened or not
  if(!result)	//If the file isn't opened
    {
      cout<<"Error:File does not exist.\n";
      return 0;	//Ends program
    }
  inf >> TheWords;	//Puts file into wordlist
  regulateOutput(TheWords);	//Starts output options and loop
  inf.close();		//Closes the stream
  return 0;
}

//Functions

/*************************************************************************/
/*																		*/
/* Function name: 	openFile											*/
/* Description: 	Opens determined file								*/
/* Parameters: 		inf/IMPORT - File to be opened						*/
/* Return Value:	bool - Returns file failure or success				*/
/* 																		*/
/*************************************************************************/
bool openFile(ifstream &inf)
{
  string filename;	//Holds filename
  cout<<"Enter the data file name >";
  cin>>filename;	//User enters filename
  inf.open(filename.c_str());	//Opens file
  return(inf);	//Returns result
}

/*************************************************************************/
/*																		*/
/* Function name: 	SelectedCharacter									*/
/* Description: 	User chooses output they desire						*/
/* Return Value:	char - User chosen character						*/
/* 																		*/
/*************************************************************************/
char menu()
{
  char SelectedCharacter;	//Declares character variable
  cout<<"\n";
  cout<<"A)ll Words in File with their Multiplicity."<<endl;
  cout<<"P)rint all Words Appearing N Times"<<endl;
  cout<<"S)ubstring: First N letters of Each Word"<<endl;
  cout<<"F)ind a Word "<<endl;
  cout<<"M)Characters of Words Appearing N Times"<< endl;
  cout<<"R)emove a Word"<<endl;
  cout<<"Q)uit"<<endl;
  cin>>SelectedCharacter;	//User inputs a character
  return SelectedCharacter;	//Character is returned
}

/*************************************************************************/
/*																		*/
/* Function name: 	numberPrompt										*/
/* Description: 	User inputs a number								*/
/* Return Value:	int - inputted number								*/
/* 																		*/
/*************************************************************************/
int numberPrompt()
{
  int numberValue=0;	//Declare int variable set to 0
  cout<<"\n";
  cout<<"Enter an integer value >";
  cin>>numberValue;		//User inputs number
  return numberValue;	//Number is returned
}

/*************************************************************************/
/*																		*/
/* Function name: 	stringPrompt										*/
/* Description: 	User inputs a string								*/
/* Return Value:	string - user inputted string						*/
/* 																		*/
/*************************************************************************/
string stringPrompt()
{ string word;	//Declare string variable
  cout<<"\n";
  cout<<"Enter a word >" ;
  cin>>word;	//User inputs word
  return word;	//Word is returned
}

/*************************************************************************/
/*																		*/
/* Function name: 	find												*/
/* Description: 	Searches for a chosen word in the wordlist			*/
/* Parameters: 		TheWords/IMPORT - Wordlist used in program			*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void find(const WordList &TheWords)
{ string findWord=stringPrompt();	//Holds user chosen word
  listItr<WordRec> it(TheWords);	//Creates a copy of the WordList
  bool found=false;					//Search is default false
  cout << endl;
  for (it.start();it.more();it.next())	//For every node
    { 
      if (it.value().getWord()==findWord)	//If the element is the same as given word
	{ cout<< it.value().getWord() << " appeared " << //Display the word and count
				it.value().getCount() << " times in the file\n";
	  found=true;							//Return that it is found
	}
    }
    if (!found)	//If not found
      cout<<"The word " << findWord <<" does not exist in the file." << endl;
}

/*************************************************************************/
/*																		*/
/* Function name: 	substring											*/
/* Description: 	Displays the first n characters of each word		*/
/* Parameters: 		TheWords/IMPORT - Wordlist to be shortened			*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void substring(const WordList &TheWords)
{ int number=numberPrompt();	//User inputs a number
  while (number<=0)				//If the number is not greater than zero
    {cout<<"\n";
      cout<<"Error: Invalid Entry"<<endl;
      number=numberPrompt();	//Ask for another number
    }
  listItr<WordRec> it(TheWords);//Create a copy of the WordList for display
  cout<<"\n";
  cout<<"Substring:\n\n";
  cout<<setw(15)<<"Words"<<endl;
  for (it.start();it.more();it.next())	//For every node
    cout<< setw(15) << it.value()(number) << endl;	//Display each word's first n characters
}
       
/*************************************************************************/
/*																		*/
/* Function name: 	mult												*/
/* Description: 	Displays each word with n count						*/
/* Parameters: 		TheWords/IMPORT - Wordlist to be searched and		*/
/*					displayed											*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void mult(const WordList &TheWords)
{ int number=numberPrompt();	//Ask for a number
  while (number<=0)				//If the number is not greater than zero
    {
      cout<<"\n";
      cout<<"Error: Invalid Entry"<<endl;
      number=numberPrompt();	//Enter a new number
    }
  listItr<WordRec> it(TheWords);//Create a WordList copy for display
  bool print=false;
  for (it.start();it.more();it.next())	//For every node
    { if (it.value().getCount()==number)//If the count is equal to the number
	{ if(!print)
	    { cout<<"\n";
	      cout<<"Words appearing "<<number<<" times:\n\n";
	      cout<<setw(15)<<"Words"<<endl;
	      print=true;
	    }
	  cout<<it.value() << endl;	//Display the word
	}
    }
  if(!print)	//If nothing was printed
    { cout<<"\nNo word in the file has this multiplicity.\n";
    }
}
 
//Print the first n characters of the words from the file 
//	that appeared m times
/*************************************************************************/
/*																		*/
/* Function name: 	multMchars											*/
/* Description: 	Mixture of the mult and substring functions			*/
/* Parameters: 		TheWords/IMPORT - Wordlist to be searched and		*/
/*					displayed											*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void multMchars(const WordList &TheWords)
{ listItr<WordRec> it(TheWords);	//Displayable copy of WordList is made
  double number;
  cout << "Enter double in form n.m >";
  cin >>  number;	//User inputs two numbers
  //Searches for things with count equal to the first number
  int mult=int(number),numChars=((number+0.001-mult)*10);
  bool print=false;	//Default nothing is printer
  for (it.start();it.more();it.next())	//For every node
    { if (it.value().getCount()==mult)	//If the count is equal to first number
	{ if(!print)
	    { cout<<"\n";
	      cout<<"First "<<numChars<<" characters of words appearing "
							   <<mult<<" times:\n";
	      cout<<setw(15)<<"Words"<<endl;
	      cout<<"\n";
	      print=true;
	    }
	  //Display the word with the first x characters, where x is the second number, and it's count
	  cout<<setw(15)<<it.value()(numChars) << endl;	
	}
    }
  if(!print)	//If nothing was printed
    { cout<<"\n";
      cout<<"No word in the file has this multiplicity.";
    }
}

/*************************************************************************/
/*																		*/
/* Function name: 	findWordInList										*/
/* Description: 	Searches for a word and its count					*/
/* Parameters: 		TheWords/IMPORT - Wordlist to be searched			*/
/*					word/IMPORT - Chosen word to be searched			*/
/* Return Value:	WordRec* - Found WordRec							*/
/* 																		*/
/*************************************************************************/
WordRec* findWordInList(const WordList &TheWords,string word)
{ listItr<WordRec> it(TheWords);	//Creates a copy of the WordList
  bool flag=false;	//Default failure
  for (it.start();it.more();it.next())	//For every node
    if (it.value().getWord()==word)		//If it's word is equivalent to chosen word
      return(&it.value());				//Return a pointer to the found WordRec
  return(0);				// Return NULL
}

/*************************************************************************/
/*																		*/
/* Function name: 	removeWord											*/
/* Description: 	User chooses how many instances of a word should be	*/
/*					removed												*/
/* Parameters: 		TheWords/IMPORT/EXPORT - Wordlist to be edited		*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void removeWord(WordList &TheWords)
{	string word; 
	cout << "\n";
	cout << "Enter the Word to Remove >";
	cin>>word;	//User inputs word
	cin.clear();
	cin.ignore(1000, '\n');
	WordRec* holder=TheWords.find(word);	//Word is searched for
	if(holder!=NULL){						//If word was found
		cout<<endl<<(*holder)<<endl<<"Delete how many occurences: ";
		int number=-1;
		cin>>number;		//User inputs number of occurences
		cin.clear();
		cin.ignore(1000, '\n');
		while (number<0){			//In the case a valid number isn't chosen
			number=-1;
			cout<<endl<<"Invalid number. Please enter a new one: "<<endl;
			cin>>number;		//User inputs a new number
			cin.clear();
			cin.ignore(1000, '\n');
		}
		if ((*holder).getCount()==number){	//If that number equal to the occurences,
			TheWords.remove(word);			//Remove the WordRec.
			cout<<endl<<"Word removed from the list."<<endl;
		}
		else if(number>(*holder).getCount()){
			cout<<endl<<"Too many occurences to remove, exiting removal."<<endl;
		}
		else{								//Assumes amount removed is less than it has,
			(*holder).setCount((*holder).getCount()-number);	//Reduce the count by the number.
			cout<<endl<<number<<" occurences removed."<<endl;
		}
	}
	else if(holder==NULL){	//Word wasn't found
		cout<<endl<<"Word not found."<<endl;
	}
	else{}
}

/*************************************************************************/
/*																		*/
/* Function name: 	regulateOutput										*/
/* Description: 	User chooses output									*/
/* Parameters: 		TheWord/IMPORT - The wordlist						*/
/* Return Value:	Void												*/
/* 																		*/
/*************************************************************************/
void regulateOutput(WordList &TheWords)
{ int number;					//Declare number variable
  string wordFind;				//Declare string variable
  char inputCharacter=menu();	//Menu opens and user chooses a desired output
  while (inputCharacter!='Q' && inputCharacter!='q')	//Loops until user quits
    {
      switch(inputCharacter)	//Switch to determine output
	{
	case 'a':
        case 'A':
		  //Displays all words and their counts
          cout << setw(15) << "Word" << setw(18) << "Appearances\n" << TheWords;
          break;
        case 'p':
        case 'P':
	  mult(TheWords);	//Prints words with chosen count
          break;
	case 's':
	case 'S':
	  substring(TheWords);	//Displays words as shorter than original
	  break;
        case 'f':
        case 'F':
	  find(TheWords);	//Searches for chosen word
          break;
	case 'M':
	case 'm':
	  multMchars(TheWords);	//Prints all words with chosen count, but the words are shorter
	  break;
		case 'R':
		case 'r':
			removeWord(TheWords);	//Removes chosen word
			break;
        default:	//Incase no valid option was chosen.
	  cout<<"\n";
          cout<<"Error: Invalid Entry"<<endl;
          break;
        }
      inputCharacter=menu();	//User chooses another option
    }
}

/*************************************************************************/
/*																		*/
/* Function name: 	ifstream overloaded operator <<						*/
/* Description: 	Inputs data from filestream to the LinkedList		*/
/* Parameters: 		inf/IMPORT - Filestream of text file.				*/
/*					right/IMPORT/EXPORT - LinkedList of WordRecs		*/
/* Return Value:	ifstream											*/
/* 																		*/
/*************************************************************************/
ifstream &operator>>(ifstream &inf, WordList &right)
{
	string tempWord;//Holder for the next word in file.
	while (inf>>tempWord){	//Inputs word from file.
		WordRec* holder=right.orderedInsert(tempWord);	//Inputs word in LinkedList.
		if(holder!=NULL)//If input function returned input failure,
			(*holder)++;//Increases counter for duplicate word by one.
	}
	return(inf);	//Returns the stream.
}

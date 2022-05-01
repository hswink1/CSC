/******************************************************************

     Author:        Henry Winkleman
     Major:         Computer Science
     Creation Date: 04/11/18
     Course:        CSC135-020
     Filename:      lab4-file-read-char_Winkleman.cpp
     Purpose:       Practice read from file: InData.txt
                    character at a time and immediately 
                    echo the character to the screen

******************************************************************/

#include <cstdlib>     // for the definition of EXIT_FAILURE
#include <stringstream>     // required for external file streams
#include <iostream>    // for cin & cout
using namespace std;


#define inFile "InData.txt"
#define newLn '\n'

int readString(ifstream&);

int main()
{

   
   int StringCount = 0;
   istringstream ins;      // ins is as an input stream

   // Open input, exit on any error.
   ins.open(inFile);  // connects ins to file inFile

   if (ins.fail())
   {
      cerr << "*** ERROR: Cannot open " << inFile 
           << " for input." << endl;
      return EXIT_FAILURE;	// failure return
   }  // end if
 
   StringCount = readString(ins);   // Read string from file  

   ins.close();  // close input file stream
   
   cout << "\n\n===> Read in total " << StringCount << " strings from file " << inFile << ".\n";
   cout << "Note: strings count including spaces but not end of line or end of file." << endl << endl;
	
   return 0;   	// successful return
}

int readString(ifstream& ins)
{

   string nextStr;       
   int StringCount = 0;   

   ins.get(nextStr);
   while (!ins.eof())
   {
	  cout << nextStr;
	  
	  if (nextStr != newLn)
      StringCount++;
	  else 
	
      ins.get(nextStr);
   }

   return StringCount;

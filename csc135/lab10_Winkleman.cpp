// lab10-file-read-write.cpp
// Read from inFile
// Search for word "the" and replace with "[thethe]"
// Output result to outFile
//
// Associate stream objects with external file names
//

#include <cstdlib>     // for the definition of EXIT_FAILURE
#include <fstream>     // required for external file streams
#include <iostream>    // for cin, cout
#include <sstream>     // for istringstream
using namespace std;

//Prototypes
string declarefile(string);

int main()
{
   // Local variables
   string inType = "input";
   string outType = "output";
   string inFile = declarefile(inType);
   string outFile = declarefile(outType);
   
   int lineCount = 0; // number of lines processed
   string line, aString;
   ifstream ins;      // ins is an input stream
   ofstream outs;      // out is an output stream

	/* -------------------------------------------------------------------------
		File open function pre C++11 version is required C string as parameter.
		C strings are just an array of normal chars (char*). 
		Use the c_str() function to convert a standard string to a C string, .
	----------------------------------------------------------------------------- */

   // Open input and output file, exit on any error.
   ins.open(inFile.c_str());  // connects ins to file inFile
   if (ins.fail())
   {
      cerr << "*** ERROR: Cannot open " << inFile 
           << " for input." << endl;
      return EXIT_FAILURE;	// failure return
   }  // end if
 
    cout << "Lab Exercise reading and writing files\n";
	  outs.open(outFile.c_str());
	
   // whitespace (carriage returns, tabs, spaces) is ignored by input operator.
   // Read file, line by line
   while (getline(ins, line))
   {
	  lineCount++;
	  
	  /*----------------------------------------------------------------------------
	     Not a good idea to mix getline and input operator for files, make use of stringstream
	     stringstream - allows you to treat a string object like a stream	  
	     stringstream - a convenient way to manipulate strings as independent I/O device
	     istringstream - input, ostringstream - output, stringstream for both
	     ----------------------------------------------------------------------------*/
	  
	  // turn line (type of string) into issLine (type of istringstream)
	  // issLine is an object of type istringstream
	  istringstream issLine(line);
	  
	  while (issLine >> aString)  // issLine can use input operator
	  {
		// do something with aString
		if (aString == "the")
		  outs << '[' << aString + aString << "] ";
		else 
		  outs << aString << ' ';
	  
	  } // end while
	  
	  outs << endl;  
	  
   }  // end while
  
   ins.close();  // close input file stream
   outs.close(); // close output file stream
   
   cout << "Read from file: " << inFile << endl;
   cout << "Output to file: " << outFile << endl;
   cout << "Check your " << outFile << endl;
	
   return 0;   	// successful return
}

string declarefile(string type){
	string file;
	cout<<"Please enter the name of your "<<type<<" file:";
	cin>>file;
	return file;
}
// File: testTerm.cpp
// Author: Dr. Spiegel
// Test all aspects of the Term class

#include <iostream>
#include <fstream>
#include "term.h"

using namespace std;

int main()
{  Term T1(5,8),T2;
   cout << "Created two Terms:\nTerm 1:" << T1 << "\nTerm 2:" << T2 << endl;
   cout << "Give Term 2 coefficient 6 and exponent 2:";
   Term *temp=new Term(6,2);
   T2=*temp;
   cout << T2 << endl;
   cout << "Multiply Term 1's coeff by 2:";
   // T1*=2;
   delete temp;
   temp=new Term(10,8);
   T1=*temp;
   cout << T1 << endl;
   cout << "Evaluate Term 2 for: \nx=1: " << T1(1) << "\nx=2:" << T1(2) << endl;
   cout << "Is " << T1 << " less than 5" << "?  Result: " << (T1<5) << endl;
   cout << "Is " << T1 << " equal to " << T2 << "?  Result: " << (T1==T2) << endl;
   cout << "\nNow check when they should be true:\n";
   cout << "Change Term 1 to 6x^2\n";
   delete temp;
   temp=new Term(6,2);
   T1=*temp;
   cout << "Is " << T1 << " equal to " << T2 << "?  Result: " << (T1==T2) << endl;
   cout << "Is " << T1 << " less than 5" << "?  Result: " << (T1<5) << endl;
}
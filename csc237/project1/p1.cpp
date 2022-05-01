/************************************************************/
/* Contributor: Henry Winkleman 							              */
/* Edit Date: September 20, 2019 							              */
/* Due Date: September 25, 2019 						              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #1										                      	*/
/* Filename: p1.cpp 									                     	*/
/* Purpose: This program recieves an assortment of numbers	*/
/*	from a text file, then presents them as a coefficient  	*/
/*	and exponent of "x". The user is allowed to choose how	*/
/*	the data is stored and sorted.                          */
/************************************************************/

#include <iostream>
#include <fstream>
#include <list>
#include <math.h>
#include "term.h"

using namespace std;

//Universal functions
bool openIFStream (ifstream &ifs, const bool &batch, string &filename);
int menu(double &x);

//Parser functions
void arrayParser(ifstream &ifs, double coefficientArray[], int exponentArray[], int &elts, const int &maxSize);
void termParser(ifstream &ifs, Term termArray[], int &elts, const int &maxSize);
void listParser (list<Term> &linkedList, ifstream &ifs);

//Output functions
void parallelIterative(const double coefficientArray[], const int exponentArray[], const int &elts, const double &x);
void parallelRecursive(const double coefficientArray[], const int exponentArray[], int &elts, const double &x, double &total, bool &firstRun);
void termArrayIterative(const Term termArray[], const int &elts, const double &x);
void termArrayRecursive(const Term termArray[], int &elts, const double &x, double &total, bool &firstRun);
void termArrayPointerRecursive(const Term termArray[], const double &x, double &total, bool &firstRun, Term* &f, const Term* &b);
void listForLoop(list<Term> linkedList, const double &x);
void listIterator(list<Term> linkedList, const double &x);

int main(int argc, char **argv){

    ifstream ifs;           //Declares stream for operation.
    const int maxSize=10;   //Sets max size for arrays.
    int elts;               //Sets counter for how many slots are used in arrays.
    double x;               //Value for 'x'.
    string filename;
    bool batch=true;

  if(argc==1){
    batch=false;
    if (!openIFStream(ifs, batch, filename)){//If stream doesn't open, closes the program.
        return 0;
    }
    while(true){
        switch(menu(x)){ //Performs what the user requested.
            case 1: {
                cout<<"**************************************************************"<<endl;
                cout<<"Option 1 chosen, parallel array iterative."<<endl<<endl;
                elts=0;
                double coefficientArray[maxSize]={0};
                int exponentArray[maxSize]={-1};
                arrayParser(ifs, coefficientArray, exponentArray, elts, maxSize);
                parallelIterative(coefficientArray, exponentArray, elts, x);
                cout<<"**************************************************************"<<endl;
                }
                break;
            case 2: {
                cout<<"**************************************************************"<<endl;
                cout<<"Option 2 chosen, parallel array recursive."<<endl<<endl;
                elts=0;
                double coefficientArray[maxSize]={0};
                int exponentArray[maxSize]={-1};
                double total=0;
                bool firstRun=true;
                arrayParser(ifs, coefficientArray, exponentArray, elts, maxSize);
                parallelRecursive(coefficientArray, exponentArray, elts, x, total, firstRun);
                cout<<"**************************************************************"<<endl;
                }
                break;
            case 3: {
                cout<<"**************************************************************"<<endl;
                cout<<"Option 3 chosen, object array iterative."<<endl<<endl;
                elts=0;
                Term termArray[maxSize];
                termParser(ifs, termArray, elts, maxSize);
                termArrayIterative(termArray, elts, x);
                cout<<"**************************************************************"<<endl;
                }
                break;
            case 4: {
              cout<<"**************************************************************"<<endl;
              cout<<"Option 4 chosen, object array recursive."<<endl<<endl;
              elts=0;
              Term termArray[maxSize];
              double total=0;
              bool firstRun=true;
              termParser(ifs, termArray, elts, maxSize);
              termArrayRecursive(termArray, elts, x, total, firstRun);
              cout<<"**************************************************************"<<endl;
              }
              break;
            case 5: {
                cout<<"**************************************************************"<<endl;
                cout<<"Option 5 chosen, object array pointer recursive."<<endl<<endl;
                elts=0;
                Term termArray[maxSize];
                double total=0;
                bool firstRun=true;
                termParser(ifs, termArray, elts, maxSize);
                Term& front=termArray[0];
                Term& back=termArray[elts-1];
                Term* f=&front;
                const Term* b=&back;
                termArrayPointerRecursive(termArray, x, total, firstRun, f, b);
                cout<<"**************************************************************"<<endl;
                }
                break;
            case 6: {
                cout<<"**************************************************************"<<endl;
                cout<<"Option 6 chosen, STL list for loop."<<endl<<endl;
                list<Term> linkedList;
                listParser(linkedList, ifs);
                listForLoop(linkedList, x);
                cout<<"**************************************************************"<<endl;
                }
                break;
            case 7: {
                cout<<"**************************************************************"<<endl;
                cout<<"Option 7 chosen, STL list iterator."<<endl<<endl;
                list<Term> linkedList;
                listParser(linkedList, ifs);
                listIterator(linkedList, x);
                cout<<"**************************************************************"<<endl;
                }
                break;
            case 8: {
                cout<<"**************************************************************"<<endl;
                cout<<"Exiting program as requested."<<endl;
                cout<<"**************************************************************"<<endl;
                }
                return 0;
        }
        ifs.clear();
        ifs.seekg(0, ios::beg);
    }
  }
  else{
    batch=true;
    filename=argv[1];
    x=atof(argv[2]);
    if (!openIFStream(ifs, batch, filename)){//If stream doesn't open, closes the program.
        return 0;
    }
    for(int batchSwitch=1;batchSwitch<=8;batchSwitch++){
    switch(batchSwitch){ //Performs what the user requested.
        case 1: {
            cout<<"**************************************************************"<<endl;
            cout<<"Option 1 chosen, parallel array iterative."<<endl<<endl;
            elts=0;
            double coefficientArray[maxSize]={0};
            int exponentArray[maxSize]={-1};
            arrayParser(ifs, coefficientArray, exponentArray, elts, maxSize);
            parallelIterative(coefficientArray, exponentArray, elts, x);
            cout<<"**************************************************************"<<endl;
            }
            break;
        case 2: {
            cout<<"**************************************************************"<<endl;
            cout<<"Option 2 chosen, parallel array recursive."<<endl<<endl;
            elts=0;
            double coefficientArray[maxSize]={0};
            int exponentArray[maxSize]={-1};
            double total=0;
            bool firstRun=true;
            arrayParser(ifs, coefficientArray, exponentArray, elts, maxSize);
            parallelRecursive(coefficientArray, exponentArray, elts, x, total, firstRun);
            cout<<"**************************************************************"<<endl;
            }
            break;
        case 3: {
            cout<<"**************************************************************"<<endl;
            cout<<"Option 3 chosen, object array iterative."<<endl<<endl;
            elts=0;
            Term termArray[maxSize];
            termParser(ifs, termArray, elts, maxSize);
            termArrayIterative(termArray, elts, x);
            cout<<"**************************************************************"<<endl;
            }
            break;
        case 4: {
          cout<<"**************************************************************"<<endl;
          cout<<"Option 4 chosen, object array recursive."<<endl<<endl;
          elts=0;
          Term termArray[maxSize];
          double total=0;
          bool firstRun=true;
          termParser(ifs, termArray, elts, maxSize);
          termArrayRecursive(termArray, elts, x, total, firstRun);
          cout<<"**************************************************************"<<endl;
          }
          break;
        case 5: {
            cout<<"**************************************************************"<<endl;
            cout<<"Option 5 chosen, object array pointer recursive."<<endl<<endl;
            elts=0;
            Term termArray[maxSize];
            double total=0;
            bool firstRun=true;
            termParser(ifs, termArray, elts, maxSize);
            Term& front=termArray[0];
            Term& back=termArray[elts-1];
            Term* f=&front;
            const Term* b=&back;
            termArrayPointerRecursive(termArray, x, total, firstRun, f, b);
            cout<<"**************************************************************"<<endl;
            }
            break;
        case 6: {
            cout<<"**************************************************************"<<endl;
            cout<<"Option 6 chosen, STL list for loop."<<endl<<endl;
            list<Term> linkedList;
            listParser(linkedList, ifs);
            listForLoop(linkedList, x);
            cout<<"**************************************************************"<<endl;
            }
            break;
        case 7: {
            cout<<"**************************************************************"<<endl;
            cout<<"Option 7 chosen, STL list iterator."<<endl<<endl;
            list<Term> linkedList;
            listParser(linkedList, ifs);
            listIterator(linkedList, x);
            cout<<"**************************************************************"<<endl;
            }
            break;
        case 8: {
            cout<<"**************************************************************"<<endl;
            cout<<"Exiting program as requested."<<endl;
            cout<<"**************************************************************"<<endl;
            }
            return 0;
      }
      ifs.clear();
      ifs.seekg(0, ios::beg);
    }
  }
}

/*************************************************************************/
/*                                                                      */
/* Function name:   openIFStream                                        */
/* Description:     Opens in stream an user-inputted file.              */
/* Parameters:      ifs- Import/Export - User-inputted file.            */
/* Return Value:    bool - Checks for failure to open the file.         */
/*                                                                      */
/*************************************************************************/
bool openIFStream (ifstream &ifs, const bool &batch, string &filename){
  if(!batch){    //If not running batch mode,
    cout<<"Please enter the file to be used with this program: ";
    cin>>filename;              //User enters file.
    cout<<endl;
  }
    ifs.open(filename.c_str()); //File attempts to open.
    if(!ifs){                   //Returns failure/success.
        cout<<"File has failed to open. Exiting program."<<endl;
        return false;
    }
    return true;
}

/*************************************************************************/
/*                                                                      */
/* Function name:   menu                                                */
/* Description:     User chooses what the file should be sorted in.     */
/* Parameters:      N/A                                                 */
/* Return Value:    int - Menu option chosen.                           */
/*                                                                      */
/*************************************************************************/
int menu(double &x){
    int itemChosen;

    cout<<"This program can sort the inputted data in these data structures:"<<endl;
    cout<<"1. Parallel iterative."<<endl<<"2. Parallel recursive."<<endl;
    cout<<"3. Object array iterative."<<endl<<"4. Object array recursive."<<endl;
    cout<<"5. Object array pointer recursive."<<endl<<"6. STL list for loop."<<endl;
    cout<<"7. STL list iterator."<<endl<<"8. Exit program."<<endl<<endl;
    cout<<"Please choose the numbered function this program is to perform: ";
    cin>>itemChosen;            //User chooses an option.
    while(itemChosen<1||itemChosen>8){
        cin.ignore(1000,'\n');
        itemChosen=0;
        cout<<"A valid option has not been chosen. Please try again: ";
        cin>>itemChosen;
    }
    if(itemChosen!=8){
        cout<<endl<<"Please choose a value for 'x': ";
        cin>>x;
        cout<<endl;
    }
    return itemChosen;  //Returns selected option.
}

/*************************************************************************/
/*                                                                      */
/* Function name:   arrayParser                                         */
/* Description:     Sorts data from file into parallel array.           */
/* Parameters:      coefficientArray - Export - Adds coefficients to    */
/*                  array.                                              */
/*                  exponentArray[] - Export - Adds exponents to        */
/*                  array.                                              */
/*                  ifs - Import - The stream which holds the input data*/
/*                  elts - Export - Counts how many indexes of the array*/
/*                  is used.                                            */
/*                  maxSize - Import - Determines the max size of the   */
/*                  array.                                              */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void arrayParser(ifstream &ifs, double coefficientArray[], int exponentArray[], int &elts, const int &maxSize){
    double coefficient; //Holds coefficient.
    int exponent;       //Holds exponent.
    bool pairPlaced;    //Lets the program know if the inputs have been accounted into the array.
    while(ifs>>coefficient>>exponent){  //While the stream isn't empty,
        int idx=0;                      //Sets index value.
        pairPlaced=false;               //Resets placement flag.
        for(;(idx<elts||elts==0)&&pairPlaced==false;idx++){   //Searches every filled index.
            if(exponent==exponentArray[idx]){           //If the exponent is the same,
                coefficientArray[idx]=coefficient+coefficientArray[idx];//Add to the index.
                pairPlaced=true;                                        //Input was placed.
            }
            else if(exponent>exponentArray[idx]){                            //If the exponent is greater.
                for(int reverseIdx=(maxSize-1);reverseIdx>idx;reverseIdx--){ //Move the items in the array
                    coefficientArray[reverseIdx]=coefficientArray[reverseIdx-1];
                    exponentArray[reverseIdx]=exponentArray[reverseIdx-1];
                }
                coefficientArray[idx]=coefficient;  //Place the new values in the index now freed up.
                exponentArray[idx]=exponent;
                if(elts<maxSize){                   //Increase elements unless it is the last element.
                    elts++;
                }
                pairPlaced=true;
            }
            else{}
        }
        if(pairPlaced==false&&(idx==elts&&elts!=maxSize)){  //If the input was not placed and the array was scanned,
            coefficientArray[idx]=coefficient;              //Fill a new index coefficient.
            exponentArray[idx]=exponent;                    //Fill a new index exponent.
            elts++;
        }
    }
}

/*************************************************************************/
/*                                                                      */
/* Function name:   termParser                                          */
/* Description:     Sorts data from file into object array.             */
/* Parameters:      termArray - Export - Adds coefficients and exponents*/
/*                  to array.                                           */
/*                  ifs - Import - The stream which holds the input data*/
/*                  elts - Export - Counts how many indexes of the array*/
/*                  is used.                                            */
/*                  maxSize - Import - Determines the max size of the   */
/*                  array.                                              */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void termParser(ifstream &ifs, Term termArray[], int &elts, const int &maxSize){
    double coefficient;
    int exponent;
    bool pairPlaced, firstPlacement=true;
    Term* holderTerm;
    while(ifs>>coefficient>>exponent){  //While the stream isn't empty,
        int idx=0;
        pairPlaced=false;
        for(;(idx<elts||elts==0)&&pairPlaced==false;idx++){ //Check all the terms,
            if(exponent==termArray[idx].getExponent()){     //If they are equivalent
                holderTerm=new Term(coefficient+termArray[idx].getCoefficient(), exponent);
                termArray[idx]=*holderTerm; //Replace the old term with the new one with new coefficient.
                pairPlaced=true;
                if(firstPlacement){         //Increase the elements if it is the first run to bypass the default value.
                    elts++;
                    firstPlacement=false;
                }
            }
            else if(exponent>termArray[idx].getExponent()){ //If the exponent is greater,
                for(int reverseIdx=(maxSize-1);reverseIdx>idx;reverseIdx--){  //Move all terms.
                    holderTerm=new Term(termArray[reverseIdx-1].getCoefficient(), termArray[reverseIdx-1].getExponent());
                    termArray[reverseIdx]=*holderTerm;
                }
                holderTerm=new Term(coefficient, exponent);
                termArray[idx]=*holderTerm; //Place term in the cleared space.
                if(elts<maxSize){           //Only increase elements if it is not the last element.
                    elts++;
                }
                pairPlaced=true;
            }
            else{}
        }
        if(pairPlaced==false&&(idx==elts&&elts!=maxSize)){  //If the input was not placed and the array was scanned,
            holderTerm=new Term(coefficient, exponent);
            termArray[idx]=*holderTerm;                     //Create a new term.
            elts++;
        }
    }
}

/*************************************************************************/
/*                                                                      */
/* Function name:   listParser                                          */
/* Description:     Sorts data from file into STL list.                 */
/* Parameters:      linkedList - Export - Holds nodes of type Term which*/
/*                  hold values for coefficient and exponent.           */
/*                  ifs - Import - The stream which holds the input data*/
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void listParser (list<Term> &linkedList, ifstream &ifs){
    double coefficient; //Holds the value for the new coefficient.
    int exponent;       //Holds the value for the new exponent.
    Term* holder;       //Holds term until inputted.
    bool firstRun=true;
    while(ifs>>coefficient>>exponent){         //While the stream is not empty,
      holder=new Term(coefficient, exponent);//Puts stream into term.
      if(firstRun){
          firstRun=false;
          linkedList.push_back(*holder);
      }
      else{
        for(int idx=0, maxSize=linkedList.size();idx<=maxSize;idx++){ //For all linked terms,
            if(*holder==linkedList.front()){                          //If the terms have the same exponent,
                //Create a new term with the two coefficients added together.
                holder=new Term((coefficient+linkedList.front().getCoefficient()), exponent);
                linkedList.pop_front();
              }
            else{
                linkedList.push_back(linkedList.front()); //Push the front node,
                linkedList.pop_front();                   //And clear original space.
            }
        }
        linkedList.push_back(*holder);         //Pushes the term to the end of the list.
      }
    }
    linkedList.sort();    //Sort.
    linkedList.reverse(); //Reverse the sort order.
}

/*************************************************************************/
/*                                                                      */
/* Function name:   parallelIterative                                   */
/* Description:     Displays the array in an iterative fashion.         */
/* Parameters:      coefficientArray - Import - Adds coefficients to    */
/*                  array.                                              */
/*                  exponentArray[] - Import - Adds exponents to        */
/*                  array.                                              */
/*                  elts - Import - Counts how many indexes of the array*/
/*                  is used.                                            */
/*                  x - Import - The value of 'x'.                      */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void parallelIterative(const double coefficientArray[], const int exponentArray[], const int &elts, const double &x){
    double total=0; //Holds total of all values.
    for(int idx=0;idx<elts-1;idx++){  //For all but the last index,
      if(coefficientArray[idx]==0){}
      else{
        if(coefficientArray[idx]!=1){  //If the coefficient is not 1,
          cout<<coefficientArray[idx];//Display it.
        }
        if(exponentArray[idx]!=0){    //If the exponent is 0,
          cout<<"x";                  //Do not show 'x'.
        }
        if(exponentArray[idx]<0||1<exponentArray[idx]){ //If the exponent is not 1 or 0,
          cout<<"^"<<exponentArray[idx];                //Display it.
        }
        cout<<" + ";
        total+=(coefficientArray[idx]*pow(x, exponentArray[idx]));   //Add it to the total.
      }
    }
    //Repeat for last index with different output format.
    if(coefficientArray[elts-1]!=1){
      cout<<coefficientArray[elts-1];
    }
    if(exponentArray[elts-1]!=0){
      cout<<"x";
    }
    if(exponentArray[elts-1]<0||1<exponentArray[elts-1]){
      cout<<"^"<<exponentArray[elts-1];
    }
    total+=(coefficientArray[elts-1]*pow(x, exponentArray[elts-1]));
    cout<<" = P(x)"<<endl<<"P("<<x<<") = "<<total<<endl<<endl;
}

/*************************************************************************/
/*                                                                      */
/* Function name:   parallelRecursive                                   */
/* Description:     Displays the array in an recursive fashion.         */
/* Parameters:      coefficientArray - Import - Adds coefficients to    */
/*                  array.                                              */
/*                  exponentArray - Import - Adds exponents to          */
/*                  array.                                              */
/*                  elts - Import/Export - Counts how many indexes of   */
/*                  the array is used.                                  */
/*                  x - Import - The value of 'x'.                      */
/*                  total - Import/Export - total value of the arrays.  */
/*                  firstRun - Import/Export - Special outputs for first*/
/*                  run.                                                */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void parallelRecursive(const double coefficientArray[], const int exponentArray[], int &elts, const double &x, double &total, bool &firstRun){
    elts--;
    int eltsCopy=elts;  //A copy of the size when running the function.
  if(firstRun){         //If it is the first run,
      firstRun=false;
      parallelRecursive(coefficientArray, exponentArray, elts, x, total, firstRun);  //Start recursion.
      if(coefficientArray[eltsCopy]!=1){  //If the coefficient is not 1,
        cout<<coefficientArray[eltsCopy]; //Display it.
      }
      if(exponentArray[eltsCopy]!=0){ //If the exponent is 0,
        cout<<"x";                     //Do not show 'x'.
      }
      if(exponentArray[eltsCopy]<0||1<exponentArray[eltsCopy]){ //If the exponent is not 1 or 0,
        cout<<"^"<<exponentArray[eltsCopy];                     //Display it.
      }
      total+=(coefficientArray[eltsCopy]*pow(x, exponentArray[eltsCopy]));  //Add it to the total.
      cout<<" = P(x)"<<endl<<"P("<<x<<") = "<<total<<endl<<endl;
  }
  else{
    if(elts!=-1){
      parallelRecursive(coefficientArray, exponentArray, elts, x, total, firstRun);  //Continue recursion
      if(coefficientArray[eltsCopy]!=1){  //If the coefficient is not 1,
        cout<<coefficientArray[eltsCopy]; //Display it.
      }
      if(exponentArray[eltsCopy]!=0){    //If the exponent is 0,
        cout<<"x";                       //Do not show 'x'.
      }
      if(exponentArray[eltsCopy]<0||1<exponentArray[eltsCopy]){ //If the exponent is not 1 or 0,
        cout<<"^"<<exponentArray[eltsCopy];                     //Display it.
      }
      cout<<" + ";
      total+=(coefficientArray[eltsCopy]*pow(x, exponentArray[eltsCopy]));   //Add it to the total.
    }
  }
}

/*************************************************************************/
/*                                                                      */
/* Function name:   termArrayIterative                                  */
/* Description:     Displays the object array in an iterative fashion.  */
/* Parameters:      termArray - Import - Adds coefficients and exponents*/
/*                  to array.                                           */
/*                  elts - Import - Counts how many indexes of the array*/
/*                  is used.                                            */
/*                  x - Import - The value of 'x'.                      */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void termArrayIterative(const Term termArray[], const int &elts, const double &x){
    double total=0;   //Holds total of all values.
    for(int idx=0;idx<elts-1;idx++){  //For all but the last term,
    if(termArray[idx].getCoefficient()==0){}
    else{
      if(termArray[idx].getCoefficient()!=1){ //If the coefficient is not 1,
        cout<<termArray[idx].getCoefficient();//Display the coefficient.
      }
      if(termArray[idx].getExponent()!=0){    //If the exponent is not 0,
        cout<<"x";                            //Display 'x'.
      }
      if(termArray[idx].getExponent()<0||1<termArray[idx].getExponent()){ //If the exponent is not 0 or 1,
        cout<<"^"<<termArray[idx].getExponent();                          //Display the exponent.
      }
      cout<<" + ";
      total+=termArray[idx](x); //Add the term to the total.
    }
  }
  //Repeat for last term with different output format.
  if(termArray[elts-1].getCoefficient()!=1){
    cout<<termArray[elts-1].getCoefficient();
  }
  if(termArray[elts-1].getExponent()!=0){
    cout<<"x";
  }
  if(termArray[elts-1].getExponent()<0||1<termArray[elts-1].getExponent()){
    cout<<"^"<<termArray[elts-1].getExponent();
  }
  total+=termArray[elts-1](x);
  cout<<" = P(x)"<<endl<<"P("<<x<<") = "<<total<<endl<<endl;
}

/*************************************************************************/
/*                                                                      */
/* Function name:   termArrayRecursive                                  */
/* Description:     Displays the object array in an recursive fashion.  */
/* Parameters:      termArray - Import - Adds coefficients and exponents*/
/*                  to array.                                           */
/*                  elts - Import/Export - Counts how many indexes of   */
/*                  the array is used.                                  */
/*                  x - Import - The value of 'x'.                      */
/*                  total - Import/Export - total value of the arrays.  */
/*                  firstRun - Import/Export - Special outputs for first*/
/*                  run.                                                */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void termArrayRecursive(const Term termArray[], int &elts, const double &x, double &total, bool &firstRun){
    elts--;
    int eltsCopy=elts;  //A copy of the size when running the function.
  if(firstRun){         //If it is the first run,
      firstRun=false;
      termArrayRecursive(termArray, elts, x, total, firstRun);  //Start recursion.
      cout<<termArray[eltsCopy]<<" = P(x)"<<endl; //Display last term.
      total+=termArray[eltsCopy](x);              //Add to total.
      cout<<"P("<<x<<") = "<<total<<endl<<endl;         //Display total.
  }
  else{
    if(elts!=-1){
      termArrayRecursive(termArray, elts, x, total, firstRun);  //Continue recursion
      cout<<termArray[eltsCopy]<<" + ";   //Display term.
      total+=termArray[eltsCopy](x);      //Add to total.
    }
  }
}

/*************************************************************************/
/*                                                                      */
/* Function name:   termArrayPointerRecursive                           */
/* Description:     Displays the object array in an recursive fashion.  */
/* Parameters:      termArray - Import - Adds coefficients and exponents*/
/*                  to array.                                           */
/*                  x - Import - The value of 'x'.                      */
/*                  total - Import/Export - total value of the arrays.  */
/*                  firstRun - Import/Export - Special outputs for first*/
/*                  run.                                                */
/*                  f - Import/Export - pointer for next printable index*/
/*                  b - Import - pointer to the last index.             */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void termArrayPointerRecursive(const Term termArray[], const double &x, double &total, bool &firstRun, Term* &f, const Term* &b){
 //A copy of the size when running the function.
  if(firstRun){         //If it is the first run,
      firstRun=false;
      termArrayPointerRecursive(termArray, x, total, firstRun, f, b);  //Start recursion.
      cout<<(*b)<<" = P(x)"<<endl;          //Display last term.
      total+=(*b)(x);                       //Add to total.
      cout<<"P("<<x<<") = "<<total<<endl<<endl;   //Display total.
  }
  else{
    if(f!=b){
      cout<<(*f)<<" + ";   //Display term.
      total+=(*f)(x);      //Add to total.
      f++;
      termArrayPointerRecursive(termArray, x, total, firstRun, f, b);  //Continue recursion
    }
  }
}

/*************************************************************************/
/*                                                                      */
/* Function name:   listForLoop                                         */
/* Description:     Displays the list values and their totality.        */
/* Parameters:      linkedList - Import - Holds nodes of type Term which*/
/*                  hold values for coefficient and exponent.           */
/*                  x - Import - value for 'x'.                         */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void listForLoop(list<Term> linkedList, const double &x){
    const int maxSize=linkedList.size();  //Create a copy of the size.
    double total=0;                       //Holds total values.
    for(int idx=1;idx<maxSize;idx++){  //For all but the last term,
        cout<<linkedList.front()<<" + ";  //Output it.
        total+=linkedList.front()(x);     //Add it to the total.
        linkedList.pop_front();           //Pop it.
    }
    cout<<linkedList.front()<<" = P(x)"<<endl;  //For the last term output it.
    total+=linkedList.front()(x);
    cout<<"P("<<x<<") = "<<total<<endl<<endl;   //Output total.
}

/*************************************************************************/
/*                                                                      */
/* Function name:   listIterator                                        */
/* Description:     Displays the list values and their totality.        */
/* Parameters:      linkedList - Import - Holds nodes of type Term which*/
/*                  hold values for coefficient and exponent.           */
/*                  x - Import - value for 'x'.                         */
/* Return Value:    N/A                                                 */
/*                                                                      */
/*************************************************************************/
void listIterator(list<Term> linkedList, const double &x){
    list<Term>::iterator listItr=linkedList.begin();
    list<Term>::iterator reverseItr=linkedList.end();
    reverseItr--;
    double total=0;
    while(listItr!=reverseItr){
      cout<<*listItr<<" + ";
      total+=(*listItr)(x);
      listItr++;
    }
    cout<<*listItr<<" = P(x)"<<endl;
    total+=(*listItr)(x);
    cout<<"P("<<x<<") = "<<total<<endl<<endl;
}

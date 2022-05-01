/************************************************************/
/* Contributor: Henry Winkleman 							*/
/* Edit Date: December 3, 2018 								*/
/* Due Date: December 5, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #4											*/
/* Filename: LinkedList.h 									*/
/* Purpose: Creates a series of data nodes that both have	*/
/*	individual elements inside them and are connected in 	*/
/*	alphabetical order through pointers that allow nodes	*/
/*	to look at the data inside the next ones.				*/
/************************************************************/

#ifndef _LinkedList_
#define _LinkedList_
#include <assert.h>
#include <iostream>

using namespace std;

//Prototypes for friend functions
template <typename eltType> class LinkedList;
template <typename eltType> class listItr;

template <typename eltType> class node
{private:
  //Default for the node build
  node(eltType info, node* link = NULL ) :  data(info), next(link) {};
  eltType data;		//Data within the node
  node*   next;		//A pointer to build a link to another node
  friend class LinkedList<eltType>;	//Used with LinkedLists
  friend class listItr<eltType>;	//Used with LinkedList iterators
};

template <typename eltType> class LinkedList
{
public:
  

	LinkedList();				//Default constructor
	LinkedList(LinkedList& cl);	//Copy constructor
	~LinkedList();				//Destructor

  //Copies one LinkedList to another.
  //Mutator	- passed LinkedList/IMPORT
  LinkedList& operator=(const LinkedList &cl);

  //Checks if the LinkedList is empty.
  //Facilitator - Returned bool/EXPORT
  bool empty();
  
  //Searches for an inputted element in the list.
  //Inspector - passed element/IMPORT
  //			returned element pointer/EXPORT
  eltType* find(const eltType &elt);

  //Orders inputted elements in alphabetical order
  //Mutator - passed element/IMPORT
  //		returned element pointer/EXPORT
  eltType *orderedInsert(const eltType &elt);
  
  //Removes element from LinkedList
  //Mutator - passed element/IMPORT
  void remove(const eltType &elt);

  //Determines how long the LinkedList is.
  //Facilitator - returned int/EXPORT
  int countNodesInList() const;

 private:
 
  node<eltType>*  head;	//Pointer to first node in list
  node<eltType>*  copy(node<eltType> *);//Copies all elements in a node
  
  //Destroys and frees node in the list
  //Mutator - passed node pointer/IMPORT
  void    destroy(node<eltType> *);

  //Counts nodes in the list
  //Inspector - passed node pointer/IMPORT
  //			returned int/EXPORT
  int     countNodes(node<eltType> *) const;
  
        //Linked list to ostream
		//friend ostream& operator<< <>(ostream&, LinkedList<eltType>);

  friend class listItr<eltType>;	//Required list iterator
};

//Outputs select data
//Inspector - passed ostream/IMPORT/EXPORT
//			passed LinkedList/IMPORT
template <typename eltType>
ostream& operator<<(ostream &os,const LinkedList<eltType> &l);

template <typename eltType> class listItr	//Creates a iterator and pointer of/to a LinkedList
{
 public:
  
  listItr( LinkedList<eltType> &l);	//Makes a copy of LinkedList as an iterator.
  
  listItr(const LinkedList<eltType> &l);	//Makes a copy of a const LinkedList as an iterator.

  //Looks at iterator head
  //Mutator
  void start();
  
  //Checks if end of list was reached
  //Facilitator - Returned bool/EXPORT
  bool more() const;
  
  //If there is a next node, curr points to it
  //Mutator
  void next();
  
  //Elements in curr is returned
  //Inspector - returned element/EXPORT
  eltType &value();
  
  //Const version of previous element return
  //Inspector - returned const element/EXPORT
  const eltType &value() const;
 
 private:
  const LinkedList<eltType> &itr;	//Refers to original LinkedList
  node<eltType> *curr;				//Points at copy of LinkedList
};


#endif

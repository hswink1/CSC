// File: LinkedList.h
// Linked List class with List Iterator class

#include <assert.h>
#include <iostream>
#include "LinkedList.h"

using namespace std;

// Construct empty LinkedList
template <typename eltType> LinkedList<eltType>::LinkedList() : head(NULL)
{}

// Copy constructor. copy() does the deep copy
template <typename eltType>
LinkedList<eltType>::LinkedList(LinkedList<eltType> &cl)
{head = copy( cl.head );}

// Free all nodes
template <typename eltType> LinkedList<eltType>::~LinkedList()
{destroy(head);}

// Assignment operator: copy() does the deep copy
template <typename eltType> 
	LinkedList<eltType>&LinkedList<eltType>::operator =(const LinkedList<eltType>& cl)
{       if (this != &cl)
    {       destroy(head);
      head = copy(cl.head);
    }
  return *this;
}

// ************** Must write new version of this function *******************
// Place elt into the list in alphanumeric order
// If element is a duplicate, do not insert, return pointer to element 
//	(allowing handling of duplicate)
// Otherwise, return NULL to signify it was the first copy in the list
template <typename eltType> eltType *LinkedList<eltType>::orderedInsert(const eltType &elt)
{ 
}

// ************** Must write new version of this function *******************
// Is this element in the linked list? 
//	If so, return a pointer to the element type
//	Otherwise, return NULL to signify not found
template <typename eltType>
eltType *LinkedList<eltType>::find(const eltType &elt)
{
}

// Inline: Look into this.
template <typename eltType> inline bool LinkedList<eltType>::empty()
{return (head == NULL);}


// ********************** Must update this function *******************
// *** Make it match the prerequisite that the item will be found ******
// Remove a node in an ordered list
// Pre: Node will be found
template <typename eltType> void LinkedList<eltType>::remove(const eltType &elt)
{ 	assert( !empty() );
	node<eltType>*	p = head;
	node<eltType>*	trailp = NULL;
	while ( p != NULL && p->data != elt )
	{	trailp = p;
		p = p->next;
	}
	assert(p != NULL);		// x is not in the LinkedList
	if (p == head)
		head = head->next;	// x is first in the LinkedList
	else
		trailp->next = p->next;	// x is farther down in the LinkedList
	delete p;
}

// Remove all nodes in the linked list, starting at l
template <typename eltType> void LinkedList<eltType>::destroy(node<eltType> *l)
{       while (l != NULL)
    {       node<eltType> *doomed = l;
      l = l->next;
      delete doomed;
    }
}

// The deep copy. Copy the source list l, one node at a time
template <typename eltType>
node<eltType>* LinkedList<eltType>::copy(node<eltType> *l)
{       node<eltType>* first = NULL;    // ptr to beginning of copied LinkedList
  node<eltType>* last = NULL;     // ptr to last item insert in the copy
  if (l != NULL)
    {       assert((first=last=new node<eltType>(l->data,NULL)) != NULL);
      for (node<eltType>* source=l->next;source!=NULL;
	   source=source->next,last=last->next)
	{       last->next = new node<eltType>(source->data,NULL);
	  assert(last->next);
	}
    }
  return first;
}
// Output a linked list, using a list iterator
template <typename eltType> ostream& operator<<(ostream &os, const LinkedList<eltType> &l)
{ listItr<eltType> lt(l);
  for (lt.start();lt.more();lt.next())
    os << lt.value() << endl;
  return os;
}

// Count nodes in a linked list, starting at l
template <typename eltType> int LinkedList<eltType>::countNodes(node<eltType> *p) const
{return ((p) ?  1+countNodes(p->next) : 0);}

// Return number of nodes in *this' list
template <typename eltType> int LinkedList<eltType>::countNodesInList() const
{return(countNodes(head));}


/* ****************************************************************
************** List Iterator Implementations *******************
****************************************************************/

// Construct a list iterator. It consists of:
//       a reference to a linked list object
//       a pointer to the actual list, initially pointing to its head
template <typename eltType>
listItr<eltType>::listItr(LinkedList<eltType> &l): itr(l),curr(l.head)
{}

template <typename eltType>
listItr<eltType>::listItr(const LinkedList<eltType> &l) : itr(l),curr(l.head)
{}

// Set curr to point at itr's head
template <typename eltType> void listItr<eltType>::start()
{curr = itr.head;}

// Is curr at the end of the list?
template <typename eltType> bool listItr<eltType>::more() const
{return curr != NULL;}

// Move curr to next node
template <typename eltType> void listItr<eltType>::next() 
{assert( curr != NULL );
  curr = curr->next;
}

// Return data in curr's node. Regardless of assert(), this
//      function shouldn't be called until making sure more() returns true
template <typename eltType> eltType &listItr<eltType>::value()
{assert( curr != NULL );
  return curr->data;
}

// Return data in curr's node. Regardless of assert(), this
//      function shouldn't be called until making sure more() returns true
template <typename eltType> const eltType &listItr<eltType>::value() const
{assert( curr != NULL );
  return curr->data;
}
// The Tpp file for Linked List
#include "WordRec.h"

template class node <WordRec>;
template class LinkedList <WordRec>;
template class listItr <WordRec>;
template ostream& operator<<(ostream &os, const LinkedList<WordRec> &l);

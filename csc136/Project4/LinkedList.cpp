/************************************************************/
/* Contributor: Henry Winkleman 							*/
/* Edit Date: December 3, 2018 								*/
/* Due Date: December 5, 2018 								*/
/* Course: CSC136 020 										*/
/* Professor Name: Dr. Spiegel								*/
/* Assignment: #4											*/
/* Filename: LinkedList.cpp 								*/
/* Purpose: Creates a series of data nodes that both have	*/
/*	individual elements inside them and are connected in 	*/
/*	alphabetical order through pointers that allow nodes	*/
/*	to look at the data inside the next ones.				*/
/************************************************************/

#include <assert.h>
#include <iostream>
#include "LinkedList.h"

using namespace std;

//Construct an empty LinkedList
template <typename eltType> LinkedList<eltType>::LinkedList() : head(NULL)
{}

//Copy constructor - copy() does the deep copy
template <typename eltType>
LinkedList<eltType>::LinkedList(LinkedList<eltType> &cl)
{head = copy( cl.head );}	//Copies all linked nodes and data.

//Frees all nodes
template <typename eltType> LinkedList<eltType>::~LinkedList()
{destroy(head);}

// Assignment operator: copy() does the deep copy
template <typename eltType> 
	LinkedList<eltType>&LinkedList<eltType>::operator=(const LinkedList<eltType>& cl)
{       if (this != &cl)	//Checks if already identical.
    {       destroy(head);	//Frees all nodes.
      head = copy(cl.head);	//Creates and copies nodes from passed list.
    }
  return *this;				//Returns new LinkedList
}

//Place elt into the list in alphanumeric order
//If element is a duplicate, do not insert, return pointer to element 
//(allowing handling of duplicate)
//Otherwise, return NULL to signify it was the first copy in the list
template <typename eltType> eltType *LinkedList<eltType>::orderedInsert(const eltType &elt)
{	
	if (head==NULL)	//If there is no first node,
	{
		head=new node<eltType>(elt);	//Create a new one and input the element.
		return NULL;					//Return that element was placed.
	}
	else if (elt==head->data){	//If the element is a duplicate of the first node,
		return &(head->data);	//Return the data's location.	
	}
	else if (elt<head->data)	//If the element is less than the first node,
	{
		node<eltType>* tempPointer=new node<eltType>(elt);	//Create a new node with the element,
		tempPointer->next=head;								//Have it point to the head,
		head=tempPointer;									//Then have the head point to it, making it
		return NULL;										//the first node, finally return success.
	}
	else								//Implies the elt is greater than the first node data.
	{
		node<eltType>* p=head->next;	//Create a pointer targeting the next node.
		node<eltType>* trailp=head;		//Create a pointer that trails behind 'p'.
		while (p!=NULL)					//While there is a next node,
		{
			if (elt==p->data){	//If the element is a duplicate of the data,
				return &(p->data);	//Return the data's location.		
			}
			else if (elt<p->data)			//If the element is less than the data
			{
				node<eltType>* tempPointer=new node<eltType>(elt);	//Create a new node.
				tempPointer->next=p; //Have it point to the node it is less than.
				trailp->next=tempPointer;	//Have the previous node point to the new one.
				return NULL;				//Return success.
			}
			else			//Implies the element is greater than the data.
			{
				trailp=trailp->next;	//Move 'trailp' ahead to where 'p' was.
				p=p->next;	//'p' moves to the next node, cycle continues.
			}
		}								//Implies end of list was reached.
		p=new node<eltType>(elt);	//Create a new node on the end with the element.
		trailp->next=p;
		return NULL;					//Return success.
	}
}

//Is this element in the linked list? 
//If so, return a pointer to the element type
//Otherwise, return NULL to signify not found
template <typename eltType>
eltType *LinkedList<eltType>::find(const eltType &elt)
{
	node<eltType>* p=head;			//Creates a pointer, 'p', to first node.
	while (p!=NULL&&elt>=(p->data)){/*While 'p' is looking at a node and the element
									is not greater than it (meaning there is no hope
									of finding the element),*/
		if (elt==(p->data)){	//If the element is the same as the data,
			return &(p->data);	//Return the data's location.	
		}
		else			//If not the same,
			p=p->next;	//Move 'p' to the next node.
	}			//Implies no chance of finding the matching data.
	return NULL;//Return failure.
}

//Inline: Look into this.
//Checks if the first node is empty.
template <typename eltType> inline bool LinkedList<eltType>::empty()
{return (head == NULL);}

//Remove a node in an ordered list
//Pre: Node will be found
template <typename eltType> void LinkedList<eltType>::remove(const eltType &elt)
{
	node<eltType>*	p = head;		//Creates 'p' to the head.
	node<eltType>*	trailp = NULL;	//Creates pointer following 'p'.
	while (p->data!=elt)			//If the data in 'p' is not the element,
	{	trailp = p;					//Move 'trailp' to 'p'.
		p = p->next;				//'p' checks the next space.
	}				//Assumes element placement was found.
	if (p == head)			//If the element is in the first node,
		head = head->next;	//Head changes to the second node.
	else						//Else the node is the second or beyond.
		trailp->next = p->next;	//The previous node links to the node after the found one.
	delete p;				//Destroy node with element.
}

//Remove all nodes in the linked list, starting at l
template <typename eltType> void LinkedList<eltType>::destroy(node<eltType> *l)
{       while (l != NULL)			//While the node has an element in it,
    {       node<eltType> *doomed = l;	//Create a pointer to it
      l = l->next;					//Bridge the previous and next nodes	
      delete doomed;				//Destroy chosen node
    }
}

// The deep copy. Copy the source list l, one node at a time
template <typename eltType>
node<eltType>* LinkedList<eltType>::copy(node<eltType> *l)
{       node<eltType>* first = NULL;	//ptr to beginning of copied LinkedList
  node<eltType>* last = NULL;     		//ptr to last item insert in the copy
  if (l != NULL)							//If the node isn't empty
	//Assuming the first node can be copied,
    {       assert((first=last=new node<eltType>(l->data,NULL)) != NULL);
      for (node<eltType>* source=l->next;source!=NULL;	//For every other node in the list
	   source=source->next,last=last->next)
	{       last->next = new node<eltType>(source->data,NULL);	//Copies the data in the next node
	  assert(last->next);	//Moves to next node
	}
    }
  return first;	//Returns first copied node
}

//Outputs the linked list by using a copy
template <typename eltType> ostream& operator<<(ostream &os, const LinkedList<eltType> &l)
{ listItr<eltType> lt(l);	//Declares the iterator
  for (lt.start();lt.more();lt.next())	//For every node in the list
    os << lt.value() << endl;			//Display the internal elements
  return os;	//Returns output in batches
}

//Counts nodes in the LinkedList
template <typename eltType> int LinkedList<eltType>::countNodes(node<eltType> *p) const
{return ((p) ?  1+countNodes(p->next) : 0);}	//Adds to the counter for every next that happens

//Self-referring countNodes
template <typename eltType> int LinkedList<eltType>::countNodesInList() const
{return(countNodes(head));}


/****************************************************************
*************** List Iterator Implementations *******************
****************************************************************/

//Constructs and iterator based off of a LinkedList
//itr = pointer to original LinkedList
//curr = pointer to list iterator
template <typename eltType>
listItr<eltType>::listItr(LinkedList<eltType> &l): itr(l),curr(l.head)
{}

//Creates a constant version of a list iterator
template <typename eltType>
listItr<eltType>::listItr(const LinkedList<eltType> &l) : itr(l),curr(l.head)
{}

//Looks at iterator head
//Mutator
template <typename eltType> void listItr<eltType>::start()
{curr = itr.head;}

//Checks if end of list was reached
//Facilitator - Returned bool/EXPORT
template <typename eltType> bool listItr<eltType>::more() const
{return curr != NULL;}

//If there is a next node, curr points to it
template <typename eltType> void listItr<eltType>::next() 
{assert( curr != NULL );
  curr = curr->next;
}

//Elements in curr is returned
template <typename eltType> eltType &listItr<eltType>::value()
{assert( curr != NULL );
  return curr->data;
}

//Returns constant reference to elements in curr
template <typename eltType> const eltType &listItr<eltType>::value() const
{assert( curr != NULL );
  return curr->data;
}

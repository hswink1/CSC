/************************************************************/
/* Contributor: Henry Winkleman and Dr. Spiegel             */
/* Edit Date: October 7, 2019   							              */
/* Due Date: October 18, 2019 		   				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #2										                      	*/
/* Filename: Node.h 									                     	*/
/* Purpose: This program holds the functions for the DblLink*/
/* DblLinkItr, and Node classes.														*/
/************************************************************/

#ifndef NODE_H
#define NODE_H

using namespace std;

//Prototyping classes so friends can be made.
template <class eltType> class DblLink;
template <class eltType> class DblLinkItr;

template <class eltType> class Node
{	private:
		Node(eltType info, Node *pLink=0, Node *rLink=0)
				   : data(info), prev(pLink), next(rLink) {};
		eltType data;	//The values stored in the node.
		Node*	prev;	//The previous node in the list.
		Node*	next;	//The next node in the list.
	friend class DblLink<eltType>;
	friend class DblLinkItr<eltType>;
};

//DblLink Class
template <class eltType> class DblLink
{	public:
		//Construct doubly-linked list.
		DblLink();

		//Copy constructor
		DblLink(DblLink&);

		//Destructor
		~DblLink();

		//Assignment operator
		DblLink& operator=(const DblLink&);

		//Insert
		void insert(eltType);

		//Remove
		bool remove(eltType);


	private:
		//Head pointer.
		Node<eltType>*	DblHead;

		//Copy function
		Node<eltType>* copy(Node<eltType> *);

		//Cleaner for Destructor
		void clean(Node<eltType> *);

		//Grants access to the list iterator.
		friend class DblLinkItr<eltType>;
};

//DblLinkItr Class
template <class eltType> class DblLinkItr
{	public:
		//Constructor
		DblLinkItr(const DblLink<eltType> &DblList);

		//Starts at the beginning of the list
		void start();

		//Checks if the list is empty
		bool isEmpty();

		//Checks if the current node is the last node
		bool isLastNode();

		//Checks if the current node is the first node
		bool isFirstNode();

		//Reveals the data of the current node
		eltType operator()() const;

		//Goes to the next node
		DblLinkItr& operator++();

		//Goes to the previous node
		DblLinkItr& operator--();

	private:
		//A reference to the iterated list
		const DblLink<eltType> &DblRef;

		//The currently viewed node
		Node<eltType> *current;
};

/********************DBLLINK Functions********************/

/*********************************************************************
*   Function name:  DblLink Constructor
*   Description:   	Creates the double linked list.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType> DblLink<eltType>::DblLink() : DblHead(NULL)
{}

/*********************************************************************
*   Function name:  Copy constructor
*   Description:   	Copies the DblLink.
*   Parameters:  		toCopy - IMPORT - The list to be copied.
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType> DblLink<eltType>::DblLink(DblLink<eltType> &toCopy){
	DblHead=copy(toCopy.DblHead);
}

/*********************************************************************
*   Function name:  Destructor ~
*   Description:   	Deletes the list in its entirety.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType> DblLink<eltType>::~DblLink()
{
	clean(DblHead);
}

/*********************************************************************
*   Function name:  Assignment operator =
*   Description:   	Deletes all nodes in current list and copies the full list
*										of nodes to the changed list.
*   Parameters:  		assigned - IMPORT - the data to be entered into the node.
*
*   Return Value: 	*this - the list to be assigned to.
*********************************************************************/
template <typename eltType> DblLink<eltType>
	&DblLink<eltType>::operator =(const DblLink<eltType>& assigned)
{
	if (this!= &assigned)		//If base is not the same as the assigned,
	{
		clean(DblHead);									//Clean the base.
		DblHead=copy(assigned.DblHead);	//Copy the node values.
	}
	return *this;						//Return the newly changed list.
}

/*********************************************************************
*   Function name:  insert
*   Description:   	Defines conditions and processes for how nodes will be added
*   Parameters:  		newData - IMPORT - the data to be entered into the node.
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType>
	void DblLink<eltType>::insert(eltType newData)
{
	//If there is no first node a new node will be made the head.
	if(DblHead==NULL){
		DblHead=new Node<eltType>(newData, NULL, DblHead);
	}
	//If the node goes before the head, moves the head and creates a new node.
	else if(newData<DblHead->data){
		Node<eltType>* oldHead=DblHead;
		DblHead=new Node<eltType>(newData, NULL, DblHead);
		oldHead->prev=DblHead;
	}
	else	//All other nodes greater than the head,
	{
		Node<eltType>* searcher=DblHead->next;
		Node<eltType>* oldSearch=DblHead;
		//Check the next node until a value less than new data is found or there is
		//none left.
		while(searcher!=NULL&&searcher->data<newData)
		{
			searcher=searcher->next;
			oldSearch=oldSearch->next;
		}
		//If there is no more data, create a node at the end.
		if(searcher==NULL){
			oldSearch->next=new Node<eltType> (newData, oldSearch, NULL);
		}
		//If the value is less than the next node, create a node before it.
		else{
			oldSearch->next=new Node<eltType> (newData, oldSearch, searcher);
			searcher->prev=oldSearch->next;
		}
	}
}
/*********************************************************************
*   Function name:  remove
*   Description:   	Removes a chosen node.
*   Parameters:  		destroyData - IMPORT - the chosen data for deletion.
*
*   Return Value: 	bool - Returns false if there was no deleted value.
*********************************************************************/
template <typename eltType> bool DblLink<eltType>::remove(eltType destroyData)
{
	//If there is no first node, nothing will be destroyed.
	if(DblHead==NULL||destroyData<DblHead->data){
		return false;
	}
	//If the head is to be destroyed, change the head to the next and delete the
	//old head.
	else if(DblHead->data==destroyData){
		Node<eltType>* behead=DblHead;
		DblHead=DblHead->next;
		delete behead;
	}
	else{
		Node<eltType>* searcher=DblHead->next;
		Node<eltType>* oldSearch=DblHead;
		//Search all other possible nodes.
		while(searcher!=NULL&&searcher->data<destroyData)
		{
			searcher=searcher->next;
			oldSearch=oldSearch->next;
		}
		if(searcher==NULL){	//If the search has reached the end, nothing is destroyed.
			return false;
		}
		//If the data is found,
		else if(destroyData==searcher->data){
			oldSearch->next=searcher->next;
			if(searcher->next==NULL){}	//If there is no more nodes, destroy it.
			else{	//If there is a next,
						//destroy it and connect the next and previous together.
				(searcher->next)->prev=oldSearch;
			}
			delete searcher;
		}
		else{	//If the value is not where it should have been, nothing is destroyed.
			return false;
		}
	}
	return true;
}

/*********************************************************************
*   Function name:  copy
*   Description:   	Creates a duplicate doubly-linked list.
*   Parameters:  		tempDblHead - IMPORT - The list to be copied.
*
*   Return Value: 	Node<eltType> - The head for the new list.
*********************************************************************/
template <typename eltType>
	Node<eltType>* DblLink<eltType>::copy(Node<eltType> *tempDblHead){
		if(tempDblHead!=NULL){	//If there is a node in the list.
		Node<eltType>* beginning=NULL;//Create a pointer to the beginning.
		Node<eltType>* end=NULL;  		//Create a pointer to the end.
		//Set both the pointers to the first node.
		beginning=end=new Node<eltType>(tempDblHead->data,NULL,NULL);
		//For every other source node, add it in and move the end pointer.
		for(Node<eltType>* src=tempDblHead->next;src!=NULL;src=src->next,end=end->next){
			end->next=new Node<eltType>(src->data,end,NULL);
		}
  }
  return NULL;
}

/*********************************************************************
*   Function name:  clean
*   Description:   	Deletes all nodes in current list and removes the head.
*   Parameters:  		tempDblHead - IMPORT - The list to be cleaned.
*
*   Return Value: 	N/A
*********************************************************************/
template <class eltType>
void DblLink<eltType>::clean(Node<eltType>* tempDblHead)
{
	while (tempDblHead!=NULL)	//If there is nodes in the list.
	{
		Node<eltType> *cleaningOrder=tempDblHead;	//Set the head for deletion.
		tempDblHead=tempDblHead->next;						//Move the head.
		delete cleaningOrder;											//Delete the old head.
	}
}



/********************DBLLINKITR Functions********************/

/*********************************************************************
*   Function name:  DblLinkItr Constructor
*   Description:   	Creates the iterator to be used.
*   Parameters:  		DblList - IMPORT - the DblLink to be iterated.
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType>
DblLinkItr<eltType>::DblLinkItr(const DblLink<eltType> &DblList): DblRef(DblList), current(NULL)
{}

/*********************************************************************
*   Function name:  start
*   Description:   	Starts the iterator at beginning of the list.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType> void DblLinkItr<eltType>::start()
{
	current=DblRef.DblHead;
}

/*********************************************************************
*   Function name:  isEmpty
*   Description:   	Checks if there are nodes in the list.
*   Parameters:  		N/A
*
*   Return Value: 	bool - Returns true if empty.
*********************************************************************/
template <typename eltType> inline bool DblLinkItr<eltType>::isEmpty()
{
	return (DblRef.DblHead==NULL);
}

/*********************************************************************
*   Function name:  isLastNode
*   Description:   	Checks if there is a next node.
*   Parameters:  		N/A
*
*   Return Value: 	bool - Returns true if there is no more.
*********************************************************************/
template <typename eltType> bool DblLinkItr<eltType>::isLastNode()
{
	return (current->next==NULL);
}

/*********************************************************************
*   Function name:  isFirstNode
*   Description:   	Checks if there is no previous node.
*   Parameters:  		N/A
*
*   Return Value: 	bool - Returns true if there is no previous.
*********************************************************************/
template <typename eltType> bool DblLinkItr<eltType>::isFirstNode()
{
	return (current->prev==NULL);
}

/*********************************************************************
*   Function name:  operator ()
*   Description:   	Outputs the data for the current node.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType>
 eltType DblLinkItr<eltType>::operator()()const
{
	return current->data;
}

/*********************************************************************
*   Function name:  operator ++
*   Description:   	Moves the iterator further through the list.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType> DblLinkItr<eltType>
	&DblLinkItr<eltType>::operator++()
{
	current=current->next;
}

/*********************************************************************
*   Function name:  operator --
*   Description:   	Moves the iterator backwards through the list.
*   Parameters:  		N/A
*
*   Return Value: 	N/A
*********************************************************************/
template <typename eltType> DblLinkItr<eltType>
	&DblLinkItr<eltType>::operator--()
{
	current=current->prev;
}

#endif

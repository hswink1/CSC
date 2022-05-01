/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 26, 2019 	   		  		              */
/* Due Date: November 28, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #4										                      	*/
/* Filename: BinarySearchTree.cpp						           	    */
/* Purpose: Binary Tree ADT using linked structures.        */
/************************************************************/

#include <iostream>
#include <string>
#include <queue>
#include "BinarySearchTree.h"

using namespace std;

// Constructor
template <typename eltType> BinaryTree<eltType>::BinaryTree()
{/*********************************************************************
	*   Function name:  Constructor
	*   Description:  Creates a Binary Tree
	*   Parameters: N/A
	*
	*   Return Value: N/A
	*********************************************************************/
	root=NULL;}

// Place Element into Tree
// Returns 1 if inserted, 0 if data already in tree
// Combines a term if it is found in the tree already.
template <typename eltType>
	  int BinaryTree<eltType>::insertToTree(const eltType &data)
{/*********************************************************************
	*   Function name:  insertToTree
	*   Description:  Takes a value and adds it as a node in the tree or
	*									combines with an already existing node.
	*   Parameters: eltType data - IMPORT - the value to be added/fused.
	*
	*   Return Value: int - Returns if the value is new or was fused.
	*********************************************************************/
	if (root==NULL) { // Empty Tree
  root=new TreeNode<eltType>(data);
  return(1);
 }
 // Search for spot to put data; We only stop when we get to the bottom (NULL)
 TreeNode<eltType> *t=root,*parent;
 while(t!=NULL) {
   if (t->info==data){ // data already in Tree
	 	 t->info+=data;

     return(0);
	 }
   parent=t; // Set the trail pointer to the ancestor of where we're going
   if (data<t->info)
     t=t->left;
   else
     t=t->right;
 }
 // Found the spot; insert node.
 if (data<parent->info) parent->left=new TreeNode<eltType>(data);
 else parent->right=new TreeNode<eltType>(data);
 return(1);
}

// Search for Element in Tree
// Assumes == is defined for eltType
// Returns Ptr to Elt if Found, NULL otherwise
template <typename eltType>
	bool BinaryTree<eltType>::treeSearch(const eltType &key)
{/*********************************************************************
	*   Function name:  treeSearch
	*   Description:  Searches for a given value and returns if it was found.
	*   Parameters: eltType - IMPORT - the value to be found.
	*
	*   Return Value: bool - Returns if the value was found.
	*********************************************************************/
	TreeNode<eltType> *t=root;
 while (t && t->info != key)
   if (key < t->info)
     t=t->left;
   else
     t=t->right;
 return(t);	// auto convert int to bool
}

// Retrieve Element from Tree (leaving Tree Intact)
// Precondition: Item is in Tree
template <typename eltType>
   eltType &BinaryTree<eltType>::retrieveFromTree(const eltType &key)
{/*********************************************************************
	*   Function name:  retrieveFromTree
	*   Description:  Searches for a given value in the tree and returns it.
	*   Parameters: eltType key - IMPORT - The value to be found.
	*
	*   Return Value:eltType - The info inside the Tree node.
	*********************************************************************/
	TreeNode<eltType> *t;
 for (t=root;t->info!=key;)
  if (key<t->info) t=t->left;
  else t=t->right;
 return(t->info);
}

// Remove an Element from the tree
// Precondition: Element is in the Tree;
template <typename eltType>
      void BinaryTree<eltType>::deleteFromTree(const eltType &data)
{/*********************************************************************
	*   Function name:  deleteFromTree
	*   Description:  Removes the passed value from the Tree.
	*   Parameters: eltType data - IMPORT - the value to be removed.
	*
	*   Return Value: N/A
	*********************************************************************/
	TreeNode<eltType> *nodeWithData,*nodeToDelete,*t=root,*trailT=NULL;
 // Find spot
 while (t->info!=data) {  // safe because of precondition
  trailT=t;
  if (data<t->info) t=t->left;
  else t=t->right;
 }
 nodeWithData=t;  // Hold onto the data Node for later deletion
 // Case 1: Leaf?
 if (!(nodeWithData->left) && !(nodeWithData->right)) {
  // No Subtrees, node is leaf...Wipe it
  // Is it the root?
  if (nodeWithData==root)
    root=NULL;
  else if (trailT->right==nodeWithData) // Parent's right child
    trailT->right=NULL;
  else
    trailT->left=NULL;
  nodeToDelete=nodeWithData;    // free this at the end
 }
 else if (!(nodeWithData->left)) {
  // If 1st condition false and this one's true, there's a right subtree
  if (!trailT) { // Node to delete is root and there is no left subtree
    nodeToDelete=root;
    root=root->right;
  }
  else { // Point parent's pointer to this node to this node's right child
    if (trailT->right==nodeWithData)
      trailT->right=nodeWithData->right;
    else
      trailT->left=nodeWithData->right;
    nodeToDelete=nodeWithData;
  }
 }
 else if (!(nodeWithData->right)) {
   // If 1st 2 conditions false and this one's true, there's a left subtree
   if (!trailT) { // Node to delete is root and there is no left subtree
     nodeToDelete=root;
     root=root->left;
   }
   else {// Otherwise, move up the right subtree
     if (trailT->right==nodeWithData)
        trailT->right=nodeWithData->left;
     else
        trailT->left=nodeWithData->left;
     nodeToDelete=nodeWithData;
   }
 }
 else { // If you make it here, node has two children
   // Go to rightmost node in left subtree; we know there's a right child...
  for (trailT=nodeWithData,t=nodeWithData->left;
                                        t->right!=NULL;trailT=t,t=t->right);
  // Want to copy data from node with 0 or 1 child to node with data to delete
  // Place node data in NodeWithData
  nodeWithData->info=t->info;
   // Set the parent of source node to point at source node's left child
   //   (We know it hasn't a right child. Also ok if no left child.)
  if (trailT==nodeWithData)
    // Need to point parent correctly.
    //   See if after the we went left there was no right child
    //   If there was no right child, this is rightmost node in left subtree
    trailT->left=t->left;
  else // we did go right; after going left, there was a right child
   // rightmost node has no r. child, so point its parent at its l. child
   trailT->right=t->left;
  nodeToDelete=t;
 }
 delete nodeToDelete;
}

// Change the value of a node
template <typename eltType>
     int BinaryTree<eltType>::change(const eltType& valueToSearch,
			 eltType& newValue)
{/*********************************************************************
	*   Function name:  change
	*   Description:  Changes the value of one node.
	*   Parameters: eltType valueToSearch - IMPORT - Finds the value to change.
	*								eltType newValue - IMPORT - Value to be changed to.
	*
	*   Return Value: int - Returns if the value was new to the tree
	*********************************************************************/
	TreeNode<eltType> *t, *trailT;
	int trailCheck=0;	//Holds the past action of the node we are searching for
	 for (t=root, trailT=root;t->info!=valueToSearch;){	//Until the value is found.
	 	switch(trailCheck){	//Moves the trail based on the past action.
			case 0: break;		//Doesn't move for first run.
			case 1: trailT=trailT->left;//If the search moved left, moves trail left.
							break;
			case 2: trailT=trailT->right;//If the search moved right, moves trail right.
							break;
		}
	  if (valueToSearch<t->info){	//If value is less than the node,
			t=t->left;	//move left.
			trailCheck=1;
		}
	  else{	//If the value is greater than the node,
			t=t->right;	//move right.
			trailCheck=2;
		}
	}
	//Any case where invariance is violated by the child nodes,
	if((t->left!=NULL&&(newValue<t->left->info||newValue==t->left->info))||
			(t->right!=NULL&&(t->right->info<newValue||t->right->info==newValue))){
		//cout<<"Case 1:"<<endl;
		deleteFromTree(t->info);	//Remove the old value
		insertToTree(newValue);		//Reinsert to fit the structure
		return 0;									//Return possible fusion of nodes
	}
	//Any case where invariance is violated by the parent node,
	else if((trailCheck==1&&(trailT->info<newValue||trailT->info==newValue))
					||(trailCheck==2&&(newValue<trailT->info||newValue==trailT->info)))
	{
		//cout<<"Case 2:"<<endl;
		deleteFromTree(t->info);	//Remove the old value
		insertToTree(newValue);		//Reinsert to fit the structure
		return 0;									//Return possible fusion of nodes
	}
	else	//Invariance is not violated
	{
		//cout<<"Case 3:"<<endl;
		t->info=newValue;	//Change the node
		return 1;					//Return no fusion
	}
}


// Need Helper to Recursively Print the Tree
template <typename eltType>
     void BinaryTree<eltType>::printInorder(TreeNode<eltType> *t) const
{/*********************************************************************
	*   Function name:  printInorder
	*   Description:  Prints the tree is ascending order.
	*   Parameters: TreeNode t - IMPORT - the node to be used in the recursion.
	*
	*   Return Value: N/A
	*********************************************************************/
	if (t) {
  printInorder(t->left);
  cout << t->info << endl;
  printInorder(t->right);
 }
}

// Display Tree using InOrder Traversal
template <typename eltType> void BinaryTree<eltType>::inorder() const
{/*********************************************************************
	*   Function name:  inorder
	*   Description:  Activates the printInorder function
	*   Parameters: N/A
	*
	*   Return Value: N/A
	*********************************************************************/
	printInorder(root);}

// Need Helper to Recursively Print the Tree
template <typename eltType>
     void BinaryTree<eltType>::printRevOrder(TreeNode<eltType> *t, char symbol,
		 bool& first) const
{/*********************************************************************
	*   Function name:  printRevOrder
	*   Description:  Prints the tree is descending order
	*   Parameters: TreeNode t - IMPORT - the node used in recursion
	*								char symbol - IMPORT - character to be added to the output
	*								bool first - IMPORT - special output for first output
	*
	*   Return Value: N/A
	*********************************************************************/
	if (t) {
  printRevOrder(t->right, symbol, first);
	if(first){
		cout << t->info;
		first=false;
	}
	else{
		cout<<" "<<symbol<<" "<<t->info;
	}
  printRevOrder(t->left, symbol, first);
 }
}

// Display Tree using Reverse InOrder Traversal
template <typename eltType> void BinaryTree<eltType>::revOrder(char symbol,
			bool first) const
{/*********************************************************************
	*   Function name:  revOrder
	*   Description:  Activates the reverse order print.
	*   Parameters: symbol - IMPORT - the character to be outputed with values
	*								first - IMPORT - designates special first output
	*
	*   Return Value: N/A
	*********************************************************************/
	printRevOrder(root, symbol, first);
 cout<<endl<<endl;
}

// Need Helper to Recursively Print the Tree
template <typename eltType>
   void BinaryTree<eltType>::printPreorder(TreeNode<eltType> *t) const
//void printTheTree(TreeNode *t)
{/*********************************************************************
	*   Function name:  printPreorder
	*   Description:  Prints left side, then right side.
	*   Parameters: TreeNode t - Node used for recursion
	*
	*   Return Value: N/A
	*********************************************************************/
	if (t) {
  cout << t->info << endl;
  printPreorder(t->left);
  printPreorder(t->right);
 }
}

// Display Tree using InOrder Traversal
template <typename eltType> void BinaryTree<eltType>::preorder() const
{/*********************************************************************
	*   Function name:  preorder
	*   Description:  Used to print the left side of the tree
	*   Parameters: N/A
	*
	*   Return Value: N/A
	*********************************************************************/
	printInorder(root);}

// Need Helper to Recursively Print the Tree
template <typename eltType>
  void BinaryTree<eltType>::printPostorder(TreeNode<eltType> *t) const
//void printTheTree(TreeNode *t)
{/*********************************************************************
	*   Function name:  printPostorder
	*   Description:  Prints children first
	*   Parameters: TreeNode t - Node used for recursion
	*
	*   Return Value: N/A
	*********************************************************************/
	if (t) {
   printPostorder(t->left);
   printPostorder(t->right);
   cout << t->info << endl;
 }
}

// Display Tree using InOrder Traversal
template <typename eltType> void BinaryTree<eltType>::postorder() const
{/*********************************************************************
	*   Function name:  postorder
	*   Description:  Used to print the right side of the tree
	*   Parameters: N/A
	*
	*   Return Value: N/A
	*********************************************************************/
	printInorder(root);}

template <typename eltType> void BinaryTree<eltType>::treePrint() const
{/*********************************************************************
	*   Function name:  treePrint
	*   Description:  Activates the printing of the tree in an imagined model.
	*   Parameters: N/A
	*
	*   Return Value: N/A
	*********************************************************************/
	treePrintHelper(root);}

template <typename eltType> void BinaryTree<eltType>::
                        treePrintHelper(TreeNode<eltType> *root) const
{/*********************************************************************
	*   Function name:  treePrintHelper
	*   Description:  Prints the tree as if it were drawn.
	*   Parameters: TreeNode root - IMPORT - the start of the tree.
	*
	*   Return Value: N/A
	*********************************************************************/
	queue<TreeNode<eltType> *> Q;
 // A dummy node to mark end of level
 TreeNode<eltType> *dummy=new  TreeNode<eltType>(-1);
 if (root) {
   cout << root->info << " " << endl;
   Q.push(root->left);
   Q.push(root->right);
   Q.push(dummy);
 }
 TreeNode<eltType> *t=root;
 while (!Q.empty()) {
  t=Q.front();
  Q.pop();
  if (t==dummy) {
   if (!Q.empty())
    Q.push(dummy);
   cout << endl;
  }
  else if (t) {
   cout << t->info << " ";
   Q.push(t->left);
   Q.push(t->right);
  }
 }
}

#include "Term.h"								//Allows Term class to be used
template class BinaryTree<Term>;//Creates a special BinaryTree class that uses Term

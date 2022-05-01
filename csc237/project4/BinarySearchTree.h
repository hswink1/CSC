/************************************************************/
/* Contributor: Henry Winkleman, Dr. Spiegel	              */
/* Edit Date: November 26, 2019 	   		  		              */
/* Due Date: November 28, 2019 		  				              	*/
/* Course: CSC237-010 										                  */
/* Professor Name: Dr. Spiegel								              */
/* Assignment: #4										                      	*/
/* Filename: BinarySearchTree.h 						           	    */
/* Purpose: Binary Tree ADT using linked structures.        */
/************************************************************/

#ifndef TREE_H
#define TREE_H

 template <typename eltType> class BinaryTree;

 template <typename eltType> class TreeNode {
   /**
   * \file BinarySearchTree.h
   * \class TreeNode
   * \brief This node holds values of eltType and links to child nodes.
   **/
 private:
  eltType info;
  TreeNode<eltType> *left,*right;
  TreeNode(const eltType &data,
                        TreeNode<eltType> *lChild=NULL,TreeNode *rChild=NULL)
  {info=data;   left=lChild; right=rChild;  }

  friend class BinaryTree<eltType>;
 };

template <typename eltType> class BinaryTree {
  /**
  * \file BinaryTree.h
  * \class BinaryTree
  * \brief This data container holds values in nodes that branch from eachother
  * depending on their comparisons to earlier values.
  **/
public:

  /**
  * \brief Allows for a Binary Tree to be created.
  * \param N/A
  * \return N/A
  **/
  //Class constructor
  BinaryTree();

  /**
  * \brief Given data is sent into the binary tree and organized based on value.
  * \param data - IMPORT - Value to be added to the Tree
  * \return	int - Output based on if the data is new or not.
  **/
  // Place Element into Tree
  // Returns 1 if inserted, 0 if Data already in tree
  int insertToTree(const eltType &data);

  /**
  * \brief Looks for the given value in the Tree.
  * \param data - IMPORT - Value to be searched in the Tree
  * \return	int - Output based on if the data is new or not.
  **/
  // Search for Element in Tree
  // Assumes == is defined for eltType
  // Returns bool according to success
  bool treeSearch(const eltType &data);

/**
*   \brief Searches for a given value in the tree and returns it.
*   \param eltType key - IMPORT - The value to be found.
*   \return eltType - The info inside the Tree node.
**/
 // Retrieve Element from Tree (leaving Tree Intact)
 // Precondition: Item is in Tree
 eltType &retrieveFromTree(const eltType &data);

 /**
 * \brief Removes the passed value from the Tree.
 * \param eltType data - IMPORT - the value to be removed.
 * \return N/A
 **/
 // Remove an Element from the tree
 // Pre: Element is in the Tree
 void deleteFromTree(const eltType &data);

 /**
 * \brief Changes the value of one node.
 * \param eltType valueToSearch - IMPORT - Finds the value to change.
 * \param eltType newValue - IMPORT - Value to be changed to.
 * \return int - Returns if the value was new to the tree
 **/
 int change(const eltType& valueToSearch, eltType& newValue);

 // Display Tree using InOrder Traversal
 /**
 * \brief Activates the printInorder function
 * \param N/A
 * \return N/A
 **/
 void inorder() const;

 // Display Tree using Reverse InOrder Traversal
 /**
 * \brief Activates the reverse order print.
 * \param symbol - IMPORT - the character to be outputed with values
 * \param first - IMPORT - designates special first output
 * \return N/A
 **/
 void revOrder(char symbol, bool first) const;

 // Display Tree using PreOrder Traversal
 /**
 * \brief Used to print the left side of the tree
 * \param N/A
 * \return N/A
 **/
 void preorder() const;

 // Display Tree using PostOrder Traversal
 /**
 * \brief Used to print the right side of the tree
 * \param N/A
 * \return N/A
 **/
 void postorder() const;

 // Breadth first print
 /**
 * \brief Activates the printing of the tree in an imagined model.
 * \param N/A
 * \return N/A
 **/
 void treePrint() const;

private:

 TreeNode<eltType> *root;

 // Display Tree using InOrder Traversal
 /**
 * \brief Prints the tree is ascending order.
 * \param TreeNode t - IMPORT - the node to be used in the recursion.
 * \return N/A
 **/
 void printInorder(TreeNode<eltType> *) const;

 // Display Tree using Reverse InOrder Traversal
 /**
 * \brief Prints the tree is descending order
 * \param TreeNode t - IMPORT - the node used in recursion
 * \param char symbol - IMPORT - character to be added to the output
 * \param bool first - IMPORT - special output for first output
 * \return N/A
 **/
 void printRevOrder(TreeNode<eltType> *, char symbol, bool& first) const;

 // Display Tree using PreOrder Traversal
 /**
 * \brief Prints left side, then right side.
 * \param TreeNode t - Node used for recursion
 * \return N/A
 **/
 void printPreorder(TreeNode<eltType> *) const;

 // Display Tree using PostOrder Traversal
 /**
 * \brief Prints children first
 * \param TreeNode t - Node used for recursion
 * \return N/A
 **/
 void printPostorder(TreeNode<eltType> *) const;

 /**
 * \brief Prints the tree as if it were drawn.
 * \param TreeNode root - IMPORT - the start of the tree.
 * \return N/A
 **/
 void treePrintHelper(TreeNode<eltType> *) const;

};

#endif

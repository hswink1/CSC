Doxygen site: http://acad.kutztown.edu/~hwink972/project4/html/index.html

Design Decisions:
Broke down some of the code from the Treetest.cpp into functions, looks better
that way.

Tried to avoid as much tampering with the Term.cpp as possible (like implementing
a > operator),seemed like it would be more work than worth.

Most updates were simple and easy, as expected BinaryTree change was the hard one.
I figured there was only two types of invariance violation, by parent and by child.
I condensed as many possibilities into the if conditional in order to save time
copying code that would otherwise be handled for slightly different cases.
This does not apply to the split between the child and parent cases, I kept those
separate for testing purposes.

Luckily I remembered that change can cause coeff to drop to 0, I wouldn't be
surprised if others weren't prepared for that.

Not many bugs happened, fairly straightforward code, probably took 15 hours of
writing (over two days), including documenting and testing.

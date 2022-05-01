Doxygen Link: http://acad.kutztown.edu/~hwink972/project3/html/index.html

Regarding timing decisions:
  Since I felt it best to avoid using a helper function as much as possible
  in order to conserve space and form a more cohesive function, I decided to
  place the timers for the print call and print end inside the print. I wanted
  to put the auto begin inside an if conditional in order to trigger only on
  the first run of the recursive function, but that would set begin out of scope
  and led to issues compiling. This meant that I needed to keep it outside a
  condition, but I placed auto end inside of a conditional, such that it only
  calculated the time to run at the end of the first call to the recursive
  function. This allowed me to calculate the time to run the function while
  still being calculated in the function so I wouldn't need to place the time
  calculators outside the function.

Helper decisions:
  As for avoiding the helper, I pass a variable "count" which keeps track of
  what instance of the recursive function it is. However, I set a default value
  for count as 0 inside of TermList so I could run the function without actually
  passing anything for count, allowing me to run it as printRecursively().

Bugfixes:
  I had only one bug, and that was a size issue in the TermArrayList when
  inputting more than ten terms. After ten terms were in, it would no longer
  read the file. That is now fixed and it reads the file to completion.

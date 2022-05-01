--------------------------------------------------------------------------
-- Author: Henry Winkleman, Dr. Daniel Spiegel
-- Major: Philosophy
-- Creation Date: February 4, 2020
-- Due Date: Febuary 7, 2020
-- Course: CSC310-020
-- Professor Name: Dr. Schwesinger/Dr. Spiegel
-- Assignment: Project 1
-- Filename: p1.adb
-- Purpose: This program will create an array of integers and
-- perform a series of tasks at the users whim such as adding
-- new integers, altering existing ones, and generating the
-- general average of all integers in the array.
--------------------------------------------------------------------------

with Text_IO;
use Text_IO;

procedure p1 is

 package Number is new Integer_IO (Integer);  --Allows for integer commands.
 use Number;
 package Real_IO is new Float_IO (Float);       --Allows for float commands.
 use Real_IO;
 --Creates the IntList data type.
 type IntList is array (1..10) of Integer;


--------------------------------------------------------------------------
-- Procedure name: introduction
-- Description: Informs the user of the general design of the program.
-- Parameters: N/A
--------------------------------------------------------------------------
 procedure introduction is
 begin
 	Put("-------------------------------------------------------");
 	New_Line;
  Put("This is an Ada program designed to allow the user to");
  New_Line;
  Put("enter integers into an array and change them at will.");
  New_Line;
  Put("The array will be automatically sorted in ascending");
  New_Line;
  Put("order. Thank you for testing this program.");
  New_Line;
  Put("-------------------------------------------------------");
 	New_Line;
  New_Line;
 end introduction;

 --------------------------------------------------------------------------
 -- Function name: options
 -- Description: Presents and obtains the user's choice for program tasks.
 -- Parameters: integerList - INPUT/OUTPUT - The array of integers.
 -- usedIndexes - INPUT/OUTPUT - The counter of indexes with integers.
 -- Return: chosenOption - The user's choice in program task.
 --------------------------------------------------------------------------
 function options return Character is
   chosenOption : Character;
 begin
  New_Line;
  Put("-------------------------------------------------------");
 	New_Line;
 	Put("The user may have the program perform these actions:");
 	New_Line;
  Put("P)rint the Integer Array");
  New_Line;
  Put("L)ook for an Integer in the Array");
  New_Line;
  Put("C)hange an Integer in the Array");
  New_Line;
  Put("I)nsert a new Integer to the Array");
  New_Line;
  Put("F)ind the average of the Array");
 	New_Line;
  Put("Q)uit the program");
 	New_Line;
  New_Line;

  Put("Chosen option >");
	Get(chosenOption); --Retrieves user option.
	New_Line;
  Put("-------------------------------------------------------");
 	New_Line;
    return(chosenOption); --Returns chosen task.
 end options;

 --------------------------------------------------------------------------
 -- Procedure name: print
 -- Description: Outputs all integers in the array.
 -- Parameters: integerList - INPUT - The array of integers.
 -- usedIndexes - INPUT - The counter of indexes with integers.
 --------------------------------------------------------------------------
 procedure print(integerList: in IntList; usedIndexes : in Integer) is
 begin
   if usedIndexes=0 then  --If there is nothing in the array,
    Put("There is nothing to print from the array.");
   else
   	Put("The array: ");
   	New_Line;
    Put("Index ");
   	for idx in integerList'First .. usedIndexes loop --For all used indexes,
      Put(idx); --Output the index.
      Put(" ");
      end loop;
    New_Line;
    Put("Value ");
    for idx in integerList'First .. usedIndexes loop --For all used indexes,
      Put(integerList(idx));  --Output the corresponding integer.
      Put(" ");
      end loop;
   end if;
   New_Line;
  end print;
  --I tried utilizing a print spacing based on the integer's divisibility by 10,
  --but doing so proved too time consuming and complex than needed given your
  --statement that you would not test large numbers.


  --------------------------------------------------------------------------
  -- Procedure name: search
  -- Description: Uses binary search to find the index of a chosen integer.
  -- Parameters: integerList - INPUT - The array of integers.
  -- usedIndexes - INPUT - The counter of indexes with integers.
  --------------------------------------------------------------------------
 procedure search(integerList: in IntList; usedIndexes : in Integer) is
   chosenInt : Integer; left, searchIdx : Integer :=1;
   right : Integer := usedIndexes; found, historicSearch : Integer :=0;
 begin
   if usedIndexes=0 then  --If there is nothing in the array,
     Put("There is no integer in the array to search for.");
     New_Line;
   else
     Put("What element would you like to find >");
     Get(chosenInt);  --User chooses integer to find.
     --While the left bound is on the left and the right to the right and no
     --found result has been achieved,
     New_Line;
     while left<=right and found=0 loop
       searchIdx:=(left+right)/2; --The search begins in the middle.
       if chosenInt<integerList(searchIdx) then --If the integer would be less,
         right:=searchIdx;  --The right bound becomes the search index.
       elsif chosenInt>integerList(searchIdx) then--If the int would be greater,
         left:=searchIdx; --Left bound becomes the search index.
       else --If neither action happens,
         Put("Located index: " & Integer'image(searchIdx));
         found:=1;  --Integer was found.
         end if;
       --If the search hasn't changed in one loop,
       if searchIdx=historicSearch and found/=1 then
         if chosenInt=integerList(usedIndexes) then --Check the last index.
           Put("Located index: " & Integer'image(usedIndexes));
           found:=1;
         else --Otherwise, integer was not in the list.
           Put("Integer was not found in the array.");
           found:=1;
           end if;
         end if;
       --Set the history to the search index before change.
       historicSearch:=searchIdx;
       end loop;
     end if;
   New_Line;
 end search;

 --------------------------------------------------------------------------
 -- Procedure name: swap
 -- Description: Exchanges two integers in the array.
 -- Parameters: integerList - INPUT/OUTPUT - The array of integers.
 -- first - INPUT - The first integer to be changed.
 -- second - INPUT - The second integer to be changed.
 --------------------------------------------------------------------------
 procedure swap(integerList: in out IntList; first : in Integer; second : in
    Integer) is
    holder : Integer;
 begin
   holder:=integerList(second); --Copies the second integer in the swap.
   integerList(second):=integerList(first); --Changes the second to the first.
   integerList(first):=holder;  --Changes the first to the copy of the second.
 end swap;

 --------------------------------------------------------------------------
 -- Procedure name: sort
 -- Description: Uses bubble sort to organize the array in ascending order.
 -- Parameters: integerList - INPUT/OUTPUT - The array of integers.
 -- usedIndexes - INPUT - The counter of indexes with integers.
 --------------------------------------------------------------------------
 procedure sort(integerList: in out IntList; usedIndexes : in Integer) is
   sorted : Integer :=0;
 begin
   while sorted=0 loop  --While no clean run,
     sorted:=1;         --Assumes sorted.
     for idx in integerList'First .. usedIndexes loop --For all used indexes,
      --If it is not the last index and the number after it is lesser,
 		  if idx/=usedIndexes and then integerList(idx)>integerList(idx+1) then
        swap(integerList, idx, idx+1); --Swap the indexes.
        sorted:=0;  --Reassumes unsorted.
      end if;
      end loop;
    end Loop;
  end sort;

  --------------------------------------------------------------------------
  -- Procedure name: change
  -- Description: Alters the integer inside an index.
  -- Parameters: integerList - INPUT/OUTPUT - The array of integers.
  -- usedIndexes - INPUT - The counter of indexes with integers.
  --------------------------------------------------------------------------
 procedure change(integerList: in out IntList; usedIndexes : in Integer) is
   chosenInt, chosenIdx :Integer;
 begin
  if usedIndexes=0 then --In case there is nothing to change.
     Put("The array is empty.");
     New_Line;
  else                  --The array has something in it.
    Put("Choose an index to change >");
    Get(chosenIdx);     --User chooses the index to change.
    New_Line;
    --If user chooses an unused or impossible index,
    while chosenIdx>usedIndexes or chosenIdx<1 loop
      Put("Invalid index, please choose another one >");
      Get(chosenIdx); --User enters a new index.
      New_Line;
      end loop;
	  Put("Choose an integer to change to >");
    Get(chosenInt); --User chooses an integer to change the index to.
    New_Line;
    integerList(chosenIdx):=chosenInt;  --Changes the index integer.

    --SORT CHECK
    if usedIndexes>1 then --If there is more than one used index.
      --If the index is not the first nor the last,
      if chosenIdx>1 and chosenIdx<usedIndexes then
        --If the integer is greater than that in the next index or less than
        --the previous index,
        if chosenInt>integerList(chosenIdx+1) or
        chosenInt<integerList(chosenIdx-1) then
          sort(integerList, usedIndexes); --Sort the array.
          end if;
      --If the index is first in the array and it is greater than the second,
      elsif chosenIdx=1 and chosenInt>integerList(2) then
        sort(integerList, usedIndexes); --Sort the array.
      --If the index is the last and it is less than the previous,
      elsif chosenIdx=usedIndexes and chosenInt<integerList(chosenIdx-1) then
        sort(integerList, usedIndexes); --Sort the array.
        end if;
      end if;
  end if;
 end change;

 --------------------------------------------------------------------------
 -- Procedure name: insert
 -- Description: Fills the next possible index with a chosen integer.
 -- Parameters: integerList - INPUT/OUTPUT - The array of integers.
 -- usedIndexes - INPUT/OUTPUT - The counter of indexes with integers.
 --------------------------------------------------------------------------
 procedure insert(integerList: in out IntList; usedIndexes : in out Integer) is
   chosenInt :Integer;
 begin
  if usedIndexes=10 then  --If the array is full,
     Put("The array is full."); --Notify the user
     New_Line;
  else
	  Put("Choose an integer to insert >");
    Get(chosenInt); --User gives an integer to be inserted to the array.
    New_Line;
    usedIndexes:=usedIndexes+1; --Increase the used index counter by one.
    integerList(usedIndexes):=chosenInt;  --Insert the integer.

    --SORT CHECK
    if usedIndexes>1 then --If there is not only one index used,
      --If the index is less than the previous,
      if integerList(usedIndexes)<integerList(usedIndexes-1) then
        sort(integerList, usedIndexes); --Sort the array.
        end if;
    end if;
  end if;
 end insert;

 --------------------------------------------------------------------------
 -- Procedure name: average
 -- Description: Calculates the sum and average of the array integers as
 -- a floating point decimal.
 -- Parameters: integerList - INPUT - The array of integers.
 -- usedIndexes - INPUT - The counter of indexes with integers.
 --------------------------------------------------------------------------
 procedure average(integerList: in IntList; usedIndexes : in Integer) is
   sum : Float :=0.0;
 begin
   for idx in integerList'First .. usedIndexes loop --For all used indexes,
    sum:=sum+Float(integerList(idx)); --Add the index to the sum.
    end loop;
   sum:=sum/Float(usedIndexes); --Divide the sum by the amount of indexes.
   Put("The average is: ");
   Put(sum, 5, 3, 0); --Special format for float output.
   New_Line;
 end average;



 --------------------------------------------------------------------------
 --------------------------------------------------------------------------
 -----------------------------------MAIN-----------------------------------
 --------------------------------------------------------------------------
 --------------------------------------------------------------------------

 --Initializes array, index counter, and user choice.
 integerList : IntList;
 usedIndexes : Integer :=0;
 chosenOption : Character :='a';

begin --Starts main procedure.
 introduction;  --Explains to the user the program function.
 while chosenOption/='Q' and chosenOption/='q' Loop --While user does not quit,
 chosenOption:=options; --Prompts the user for the function to perform.
 New_Line;
 case chosenOption is
   when 'P' | 'p' =>
     Put("P)rint");
     New_Line;
     print(integerList, usedIndexes); --Print the array.
   when 'L' | 'l' =>
     Put("L)ook");
     New_Line;
     search(integerList, usedIndexes);  --Search for an integer.
   when 'C' | 'c' =>
     Put("C)hange");
     New_Line;
     change(integerList, usedIndexes);  --Change the integer in an index.
   when 'I' | 'i' =>
     Put("I)nsert");
     New_Line;
     insert(integerList, usedIndexes);  --Insert an integer into the array.
   when 'F' | 'f' =>
     Put("F)ind");
     New_Line;
     average(integerList, usedIndexes); --Find the average.
   when 'Q' | 'q' =>
     Put("Q)uit");    --Loop will end as quit character was chosen.
     New_Line;
     New_Line;
     New_Line;
   when others => --If invalid option was chosen,
     Put("Please type choose a valid option >");
     Get(chosenOption); --User enters a new choice.
   end case;
 end Loop;

end p1;

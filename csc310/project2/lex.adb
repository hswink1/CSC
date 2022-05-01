--------------------------------------------------------------------------
-- Author: Henry Winkleman
-- Major: Philosophy
-- Creation Date: March 26th, 2020
-- Due Date: March 31st, 2020
-- Course: CSC310-020
-- Professor Name: Dr. Spiegel
-- Assignment: Project 2
-- Filename: lex.adb
-- Purpose: This program is built to read a file of source code, then
-- determines whether the symbols that are read are an idenifier or other
-- fundamental part of that coding language. This is designed to read
-- the grammar provided in MiniGrammar.pdf.
--
-- Notes: The core of the program is in fileRead, handles input/output
-- with subfunctions for determining lexeme and possible errors.
-- lookAhead is similar to fileRead, but is only used in the case of a
-- possible BADID and will use that cycle until a new lexeme is read or
-- the line has ended.
--------------------------------------------------------------------------


with Ada.Text_Io;	use Ada;
with Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling;
use Text_IO;

procedure lex is

  type Char_Type is (Progsym, Decsym, Beginsym, Endsym, Typesym, Readsym,
  Writesym, Number, Identifier, Equals, Plus, Minus, Multiply, Divide, Colon,
  Semicolon, LParenthesis, RParenthesis, Comma, BADID, BADSYM, BADNUM, Blank);

  package Class_IO is new Ada.Text_IO.Enumeration_IO(Char_Type);
  use Class_IO;

--------------------------------------------------------------------------
-- Procedure name: introduction
-- Description: Informs the user of the general design of the program.
-- Parameters: N/A
--------------------------------------------------------------------------
 procedure introduction is
 begin
 	Put("-------------------------------------------------------");
 	New_Line;
  Put("This is an Ada program designed to read a user-inputted");
  New_Line;
  Put(" file. It will then read that file and determine what");
  New_Line;
  Put("lexeme type the string belongs to. Designed for the");
  New_Line;
  Put("mini grammar in MiniGrammar.pdf. Command line argument:");
  New_Line;
  Put("./lex <input file> <output file>");
  New_Line;
  Put("-------------------------------------------------------");
 	New_Line;
  New_Line;
 end introduction;

--------------------------------------------------------------------------
-- Function name: openIFile
-- Description: Opens user specified input file.
-- Parameters: iFile - INPUT/OUTPUT - filestream for input.
--
-- Credit to Dr. Spiegel,
--https://faculty.kutztown.edu/spiegel/CSc310/Programs/Project2/OpenFile.adb.txt
--------------------------------------------------------------------------
procedure openIFile(iFile :  in out File_Type) is
  chosenName : String(1..80);
  length : natural;
begin
 	New_Line;
 	Put("Please enter the file to be opened and read: ");
  get_line(chosenName, length); --User prompt for name
	open(File=>iFile, Mode=>in_file, Name=>chosenName(1..length)); --Opens file
 	New_Line;
end openIFile;

--------------------------------------------------------------------------
-- Function name: openOFile
-- Description: Opens user specified output file.
-- Parameters: oFile - INPUT/OUTPUT - filestream for output.
--
-- Credit to Dr. Spiegel,
--https://faculty.kutztown.edu/spiegel/CSc310/Programs/Project2/OpenFile.adb.txt
--------------------------------------------------------------------------
procedure OpenOFile (oFile : in out file_type) is
	   chosenName : string (1..80);
	   length   : natural;
	begin
	   Put("Please enter the file to be written to: ");
	   get_line(chosenName, length); --User prompt for file name.
     --Opens output file
	   Create(File=>oFile, Mode=>out_file, Name=>chosenName(1..length));
     New_Line;
	end openOFile;

--------------------------------------------------------------------------
-- Function name: isNumber
-- Description: Determines if every character in the string is a number.
-- Parameters: convertedString - INPUT - string of possible number sequence.
-- Return: Boolean - true if all characters are a number.
--------------------------------------------------------------------------
function isNumber(convertedString : in String) return Boolean is
begin
  for idx in convertedString'Range loop --For all characters in the string
    if convertedString(idx) /= '0' and convertedString(idx) /= '1' and convertedString(idx) /= '2' and
    convertedString(idx) /= '3' and convertedString(idx) /= '4' and convertedString(idx) /= '5' and
    convertedString(idx) /= '6' and convertedString(idx) /= '7' and convertedString(idx) /= '8' and
    convertedString(idx) /= '9' then --If they are not a number
      return (false);
    end if;
  end loop;
  return (true);
end isNumber;

--------------------------------------------------------------------------
-- Function name: badNumCheck
-- Description: Determines if a string is a number sequence or error.
-- Parameters: convertedString - INPUT - string of possible number sequence.
-- Return: Boolean - true if it is an error.
--------------------------------------------------------------------------
function badNumCheck(convertedString : in String) return Boolean is
begin
  --If the first char is a number,
  if isNumber(convertedString(convertedString'First..1)) then
     for idx in convertedString'Range loop
        if not isNumber(convertedString(idx..idx)) then --but one other is not,
          return (true);  --Return BADNUM flag.
        end if;
     end loop;
  end if;
  return (false);
end badNumCheck;

--------------------------------------------------------------------------
-- Function name: determineGrammar
-- Description: Determines the lexeme of a string.
-- Parameters: Class - OUTPUT - lexeme of the string.
-- currString - INPUT - unbounded string of characters.
--------------------------------------------------------------------------
procedure determineGrammar(Class : out Char_Type;
currString : in Ada.Strings.Unbounded.Unbounded_String) is
    --Converts unbounded string to standard string for comparison.
    convertedString : String := Ada.Strings.Unbounded.To_String(currString);
begin
  --Safety: if empty string is entered, nothing happens.
  if convertedString = "" then
    null;
  else
    if convertedString = "program" then
      Class := Progsym;
    elsif convertedString = "dec" then
      Class := Decsym;
    elsif convertedString = "begin" then
      Class := Beginsym;
    elsif convertedString = "end." then
      Class := Endsym;
    elsif convertedString = "real" or convertedString = "int" then
      Class := Typesym;
    elsif convertedString = "Read" then
      Class := Readsym;
    elsif convertedString = "Write" then
      Class := Writesym;
    else
      --Checks if string is an erroneous number sequence.
      if badNumCheck(convertedString) then
        Class := BADNUM;
      --Checks if string is a number sequence.
      elsif isNumber(convertedString) then
          Class := Number;
      else  --Assumes identifier if no other possibility met.
          Class := Identifier;
      end if;
    end if;
  end if;
end determineGrammar;

--------------------------------------------------------------------------
-- Function name: determineSym
-- Description: Determines the lexeme of a symbol
-- Parameters: Char - INPUT - holder for possible symbol.
-- symClass - lexeme of symbol.
-- wasSym - Boolean flag to determine if it was a symbol or not. Could
-- not use OUT parameters in a function vs. procedure.
--------------------------------------------------------------------------
procedure determineSym(Char : in Character; symClass : out Char_Type;
wasSym : out Boolean) is
begin
  wasSym := true; --Assumes Char is a symbol,
  if Char = '=' then
    symClass := Equals;
  elsif Char = '+' then
    symClass := Plus;
  elsif Char = '-' then
    symClass := Minus;
  elsif Char = '*' then
    symClass := Multiply;
  elsif Char = '/' then
    symClass := Divide;
  elsif Char = ':' then
    symClass := Colon;
  elsif Char = ';' then
    symClass := Semicolon;
  elsif Char = '(' then
    symClass := LParenthesis;
  elsif Char = ')' then
    symClass := RParenthesis;
  elsif Char = ',' then
    symClass := Comma;
  elsif Char in 'a'..'z' or Char in 'A'..'Z' or Char in '0'..'9' then
    wasSym := false;  --Char was not a valid symbol.
  else  --Implies Char is none of the valid symbols, nor a letter nor number.
    symClass := BADSYM;
  end if;
end determineSym;

--------------------------------------------------------------------------
-- Function name: lookAhead
-- Description: Reads the iFile and creates approriate output for oFile.
-- Parameters: iFile - INPUT - the file to be read.
-- oFile - INPUT - File to be outputted to.
-- Char - INPUT/OUTPUT - holder for obtained character.
-- Class - INPUT/OUTPUT - lexeme of the string.
-- prevClass - INPUT/OUTPUT - lexeme of previous string.
-- symClass - INPUT/OUTPUT - lexeme of a symbol.
-- currString - INPUT/OUTPUT - unbounded string to add to.
--
-- In essence, copies the cycle and variables of fileRead, but alters output.
--------------------------------------------------------------------------
procedure lookAhead(iFile : in File_Type; oFile : in File_Type;
Char : in out Character; Class : in out Char_Type; prevClass : in out Char_Type;
symClass : in out Char_Type; wasSym : in out Boolean;
currString : in out Ada.Strings.Unbounded.Unbounded_String) is
  IDCheck : Boolean := true;  --Shuts off cycle if a lexeme is evaluated.
  prevSym : Char_Type := symClass;  --Previous symbol used for string BADSYM SYM
begin
  while not End_Of_Line(iFile) and IDCheck loop --While the line is not empty,
    Get(File=>iFile, Item=>Char);   --Get the next character.

    --If the char is a space, tab or newline
    if Char = ' ' or Char = Character'Val(9) or Char = Character'Val(10)
    or Char = Character'Val(13) then
      --If there is a character in the string,
      if Ada.Strings.Unbounded.To_String(currString) /= "" then
        determineGrammar(Class, currString);  --Determine the lexeme.
        if Class = Identifier then  --If ID, confirms BADID
          Class := BADID;
          Class_IO.Put(oFile, Class); --Outputs BADID
          New_Line(oFile);
          symClass := Blank; --Resets BADSYM for no accidental BADSYM.
          IDCheck := false;   --Sets shutoff for cycle.
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset.
        else  --Assumes string was not an ID
          Class_IO.Put(oFile, prevClass); --Prints previous lexeme.
          New_Line(oFile);
          Class_IO.Put(oFile, symClass);  --Prints BADSYM
          New_Line(oFile);
          Class_IO.Put(oFile, Class); --Prints new lexeme.
          New_Line(oFile);
          symClass := Blank; --Resets BADSYM
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset.
          IDCheck := false; --Sets shutoff for cycle
        end if;
      else  --Assumes first character was a space.
        Class_IO.Put(oFile, prevClass); --Output prevous lexeme.
        New_Line(oFile);
        Class_IO.Put(oFile, symClass);  --Output BADSYM
        New_Line(oFile);
        symClass := Blank; --Resets BADSYM
        IDCheck := false;   --Sets shutoff for cycle.
      end if;

    else  --Implies the character isn't a blank space.
      determineSym(Char, symClass, wasSym);  --Determine if Char is a symbol,
      if wasSym then  --If it was a symbol,
        --And if the string is empty,
        if Ada.Strings.Unbounded.To_String(currString) = "" then
          IDCheck := false; --Sets shutoff for cycle.
          Class_IO.Put(oFile, prevClass); --Outputs previous class.
          New_Line(oFile);
          Class_IO.Put(oFile, prevSym); --Outputs previous symbol.
          New_Line(oFile);
          Class_IO.Put(oFile, symClass); --Outputs new symbol.
          New_Line(oFile);

        else  --Implies the string wasn't empty.
          determineGrammar(Class, currString);  --Determines string lexeme.
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset
          if Class = Identifier or Class = Number then  --Confirms BADID
            IDCheck := false; --Sets shutoff for cycle.
            Class := BADID;   --Sets BADID
            Class_IO.Put(oFile, Class); --Output BADID
            New_Line(oFile);
            Class_IO.Put(oFile, symClass); --Output the new symbol.
            New_Line(oFile);
            symClass := Blank; --Resets BADSYM
          else  --Assumes no BADID
            IDCheck := false;
            Class_IO.Put(oFile, prevClass); --Outputs previous lexeme.
            New_Line(oFile);
            Class_IO.Put(oFile, symClass);  --Outputs symbol lexeme.
            New_Line(oFile);
            Class_IO.Put(oFile, Class); --Outputs new string lexeme.
            New_Line(oFile);
          end if;
        end if;

      else  --Implies the Char wasn't a space or symbol,
        Ada.Strings.Unbounded.Append(currString, Char); --Add it to the string
      end if;
    end if;
  end loop;   --End of the line.
  if IDCheck then --If nothing was found in the line,
    Class_IO.Put(oFile, Class); --Output held lexeme
    New_Line(oFile);
    Class_IO.Put(oFile, symClass);  --Output BADSYM
    New_Line(oFile);
  end if;
end lookAhead;

--------------------------------------------------------------------------
-- Function name: fileRead
-- Description: Reads the iFile and creates approriate output for oFile.
-- Parameters: iFile - INPUT - the file to be read.
--------------------------------------------------------------------------
procedure fileRead(iFile : in File_Type; oFile : in File_Type) is
  currString : Ada.Strings.Unbounded.Unbounded_String;  --String to be added to.
  Char       : Character; --Holder for the most recently obtained character.
  Class      : Char_Type; --Holder for the lexeme of the current string.
  prevClass  : Char_Type; --Holder for the lexeme of the previous string.
  symClass   : Char_Type; --Holder for the lexeme of a symbol.
  wasSym     : Boolean;   --Flag for if the character was a symbol.


begin
  while not End_Of_File (iFile) loop  --While the file is not empty,
    while not End_Of_Line(iFile) loop --While the line is not empty,
      Get(File=>iFile, Item=>Char);   --Get the next character.


      --If the char is a space, tab or newline
      if Char = ' ' or Char = Character'Val(9) or Char = Character'Val(10)
      or Char = Character'Val(13) then
        prevClass := Blank;
        --If there is a character in the string,
        if Ada.Strings.Unbounded.To_String(currString) /= "" then
          determineGrammar(Class, currString);  --Determine the lexeme.
          Class_IO.Put(oFile, Class);           --Output the lexeme.
          New_Line(oFile);
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset.
        end if;


      else  --Implies the character isn't a blank space.
        determineSym(Char, symClass, wasSym);  --Determine if Char is a symbol,
        if wasSym then  --If it was a symbol,
          --And if the string is empty,
          if Ada.Strings.Unbounded.To_String(currString) = "" then
            Class_IO.Put(oFile, symClass); --Output the lexeme.
            New_Line(oFile);


          else  --Implies the string wasn't empty.
            determineGrammar(Class, currString);--Output the string lexeme
            prevClass := Class;
            if symClass /= BADSYM then
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
              Class_IO.Put(oFile, Class); --Output string lexeme.
              New_Line(oFile);
              Class_IO.Put(oFile, symClass); --Output symbol lexeme.
              New_Line(oFile);
            --Exception condition for "end."
            elsif Char = '.' and
            Ada.Strings.Unbounded.To_String(currString) = "end" then
               --Add it to the string
              Ada.Strings.Unbounded.Append(currString, Char); --Adds the '.'
              determineGrammar(Class, currString);  --Confirms as Endsym.
              Class_IO.Put(oFile, Class); --Outputs string lexeme.
              New_Line(oFile);
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
            --BADID is a possibility, runs a special cycle to confirm.
            elsif Class = Identifier then
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
              lookAhead(iFile, oFile, Char, Class, prevClass,
              symClass, wasSym, currString);
            else  --No BADID, prints as is.
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
              Class_IO.Put(oFile, Class); --Output string lexeme.
              New_Line(oFile);
              Class_IO.Put(oFile, symClass); --Output symbol lexeme.
              New_Line(oFile);
            end if;
          end if;

        else  --Implies the Char wasn't a space or symbol,
          Ada.Strings.Unbounded.Append(currString, Char); --Add it to the string
        end if;
      end if;

    end loop;   --End of the line.
    if Ada.Strings.Unbounded.To_String(currString) /= "" then
      determineGrammar(Class, currString);
      Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset
      Class_IO.Put(oFile, Class); --Outputs string lexeme.
      New_Line(oFile);
    end if;
    Skip_Line(iFile);  --Pass EOL buffer.
  end loop; --End of the file.
end fileRead;

--------------------------------------------------------------------------
--------------------------------------------------------------------------
-----------------------------------MAIN-----------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------

 --Initializes filestreams
 iFile, oFile : File_Type;

begin --Start main.
  introduction;  --Explains to the user the program function.
  if Argument_Count = 0 then
    openIFile(iFile);		--Prompts user for input file.
    openOFile(oFile);		--Prompts user for output file.
    fileRead(iFile, oFile); --Starts program function.

  elsif Argument_Count = 2 then --Command line argument.
    open(File=>iFile, Mode=>in_file, Name=>Argument(1));   --Opens input stream.
    Create(File=>oFile, Mode=>out_file, Name=>Argument(2));--Opens output stream
    fileRead(iFile, oFile); --Starts program function.
  end if;
  --Closes streams and ends program.
  Put_Line("Program complete.");
  Close(iFile);
  Close(oFile);
end lex;

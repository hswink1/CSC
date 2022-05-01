--<****h*-----------------------------------------------------------------------
-- NAME
--  lex.adb - Scanner for given language.
-- AUTHOR
--  Henry Winkleman
-- CREATION DATE
--  26 March 2020
-- DESCRIPTION
--  This package is built to read a file of source code, then
--  determines whether the symbols that are read are an idenifier or other
--  fundamental part of that coding language. This is designed to read
--  the grammar provided in MiniGrammar.pdf.
-- NOTES
--  Robodoc http://acad.kutztown.edu/~hwink972/csc310/project3.html
--------------------------------------------------------------------------->****


with Ada.Text_Io;	use Ada;
with Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling;
with objectpkg; use objectpkg;
use Text_IO;

package body lex is

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
-- Function name: outputFormat
-- Description: Adds the lexeme to a vector object.
-- Parameters: Class - INPUT - Lexeme of the token.
-- tableHolder : objectpkg.lexObject; - The object to be appended.
-- table : objectpkg.lexTable;	- Vector of lexemes to be appended to.
-- currString - INPUT - Token of lexeme
--------------------------------------------------------------------------
procedure outputFormat(Class : in Char_Type; tableHolder : in out objectpkg.lexObject;
table : in out objectpkg.lexTable; Line : in Integer;
currString : in Ada.Strings.Unbounded.Unbounded_String) is
begin
  --Alters tableHolder and appends it to the vector.
  Ada.Strings.Unbounded.Set_Unbounded_String(tableHolder.ID, "");
  tableHolder.Class := Class;
  tableHolder.Line := Line;
  tableHolder.ID := currString;
  table.lexTbl.Append(tableHolder);
end outputFormat;

--------------------------------------------------------------------------
-- Function name: lookAhead
-- Description: Reads the iFile and creates approriate output for oFile.
-- Parameters: iFile - INPUT - the file to be read.
-- tableHolder : objectpkg.lexObject; - The object to be appended.
-- table : objectpkg.lexTable;	- Vector of lexemes to be appended to.
-- oFile - INPUT - File to be outputted to.
-- Char - INPUT/OUTPUT - holder for obtained character.
-- Class - INPUT/OUTPUT - lexeme of the string.
-- prevClass - INPUT/OUTPUT - lexeme of previous string.
-- symClass - INPUT/OUTPUT - lexeme of a symbol.
-- currString - INPUT/OUTPUT - unbounded string to add to.
-- echo - boolean	- Holds a flag for raw input file text output.
--
-- In essence, copies the cycle and variables of fileRead, but alters output.
--------------------------------------------------------------------------
procedure lookAhead(iFile : in File_Type; tableHolder : in out objectpkg.lexObject;
table : in out objectpkg.lexTable; Char : in out Character; Class : in out Char_Type;
prevClass : in out Char_Type; symClass : in out Char_Type;
wasSym : in out Boolean; Line : in out Integer;
currString : in out Ada.Strings.Unbounded.Unbounded_String;
prevString : in out Ada.Strings.Unbounded.Unbounded_String; echo : in boolean) is
  IDCheck : Boolean := true;  --Shuts off cycle if a lexeme is evaluated.
  prevSym : Char_Type := symClass;  --Previous symbol used for string BADSYM SYM
begin
  while not End_Of_Line(iFile) and IDCheck loop --While the line is not empty,
    Get(File=>iFile, Item=>Char);   --Get the next character.
    if echo then  --If echo argument was entered.
      Put(Char);  --Outout character by character.
    end if;

    --If the char is a space, tab or newline
    if Char = ' ' or Char = Character'Val(9) or Char = Character'Val(10)
    or Char = Character'Val(13) then
      --If there is a character in the string,
      if Ada.Strings.Unbounded.To_String(currString) /= "" then
        determineGrammar(Class, currString);  --Determine the lexeme.
        if Class = Identifier then  --If ID, confirms BADID
          Class := BADID;
          outputFormat(Class, tableHolder, table, Line, currString);
          symClass := Blank; --Resets BADSYM for no accidental BADSYM.
          IDCheck := false;   --Sets shutoff for cycle.
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset.
        else  --Assumes string was not an ID
          outputFormat(prevClass, tableHolder, table, Line, currString);
          outputFormat(symClass, tableHolder, table, Line, currString);
          outputFormat(Class, tableHolder, table, Line, currString);
          symClass := Blank; --Resets BADSYM
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset.
          IDCheck := false; --Sets shutoff for cycle
        end if;
      else  --Assumes first character was a space.
        outputFormat(prevClass, tableHolder, table, Line, currString);
        outputFormat(symClass, tableHolder, table, Line, currString);
        symClass := Blank; --Resets BADSYM
        IDCheck := false;   --Sets shutoff for cycle.
      end if;

    else  --Implies the character isn't a blank space.
      determineSym(Char, symClass, wasSym);  --Determine if Char is a symbol,
      if wasSym then  --If it was a symbol,
        --And if the string is empty,
        if Ada.Strings.Unbounded.To_String(currString) = "" then
          IDCheck := false; --Sets shutoff for cycle.
          outputFormat(prevClass, tableHolder, table, Line, currString);
          outputFormat(prevSym, tableHolder, table, Line, currString);
          outputFormat(symClass, tableHolder, table, Line, currString);

        else  --Implies the string wasn't empty.
          determineGrammar(Class, currString);  --Determines string lexeme.
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset
          if Class = Identifier or Class = Number then  --Confirms BADID
            IDCheck := false; --Sets shutoff for cycle.
            Class := BADID;   --Sets BADID
            outputFormat(Class, tableHolder, table, Line, currString);
            outputFormat(symClass, tableHolder, table, Line, currString);
            symClass := Blank; --Resets BADSYM
          else  --Assumes no BADID
            IDCheck := false;
            outputFormat(prevClass, tableHolder, table, Line, currString);
            outputFormat(symClass, tableHolder, table, Line, currString);
            outputFormat(Class, tableHolder, table, Line, currString);
          end if;
        end if;

      else  --Implies the Char wasn't a space or symbol,
        Ada.Strings.Unbounded.Append(currString, Char); --Add it to the string
      end if;
    end if;
  end loop;   --End of the line.
  if IDCheck then --If nothing was found in the line,
    outputFormat(Class, tableHolder, table, Line, currString);
    outputFormat(symClass, tableHolder, table, Line, currString);
  end if;
end lookAhead;

--------------------------------------------------------------------------
-- Function name: fileRead
-- Description: Reads the iFile and creates approriate output for oFile.
-- Parameters: iFile - INPUT - the file to be read.
-- table : objectpkg.lexTable;	- Vector of lexemes to be appended to.
-- echo - boolean - Holds a flag for raw input file text output.
--------------------------------------------------------------------------
procedure fileRead(iFile : in File_Type; table : in out objectpkg.lexTable;
echo : in boolean) is
  currString, prevString : Ada.Strings.Unbounded.Unbounded_String;  --String to be added to.
  Char       : Character; --Holder for the most recently obtained character.
  Class      : Char_Type; --Holder for the lexeme of the current string.
  prevClass  : Char_Type; --Holder for the lexeme of the previous string.
  symClass   : Char_Type; --Holder for the lexeme of a symbol.
  wasSym     : Boolean;   --Flag for if the character was a symbol.
  Line       : Integer := 0;
  tableHolder : objectpkg.lexObject;  --Object to hold lexemes.


begin
  while not End_Of_File (iFile) loop  --While the file is not empty,
    if echo then  --If echo command,
      New_Line; --Every new line, output buffer.
    end if;
    Line := Line + 1;
    while not End_Of_Line(iFile) loop --While the line is not empty,
      Get(File=>iFile, Item=>Char);   --Get the next character.
      if echo then  --If echo command,
        Put(Char);  --Output each character.
      end if;

      --If the char is a space, tab or newline
      if Char = ' ' or Char = Character'Val(9) or Char = Character'Val(10)
      or Char = Character'Val(13) then
        prevClass := Blank;
        --If there is a character in the string,
        if Ada.Strings.Unbounded.To_String(currString) /= "" then
          determineGrammar(Class, currString);  --Determine the lexeme.
          outputFormat(Class, tableHolder, table, Line, currString);
          Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset.
        end if;


      else  --Implies the character isn't a blank space.
        determineSym(Char, symClass, wasSym);  --Determine if Char is a symbol,
        if wasSym then  --If it was a symbol,
          --And if the string is empty,
          if Ada.Strings.Unbounded.To_String(currString) = "" then
            outputFormat(symClass, tableHolder, table, Line, currString);


          else  --Implies the string wasn't empty.
            determineGrammar(Class, currString);--Output the string lexeme
            prevClass := Class;
            if symClass /= BADSYM then
              outputFormat(Class, tableHolder, table, Line, currString);
              outputFormat(symClass, tableHolder, table, Line, currString);
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
            --Exception condition for "end."
            elsif Char = '.' and
            Ada.Strings.Unbounded.To_String(currString) = "end" then
               --Add it to the string
              Ada.Strings.Unbounded.Append(currString, Char); --Adds the '.'
              determineGrammar(Class, currString);  --Confirms as Endsym.
              outputFormat(Class, tableHolder, table, Line, currString);
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
            --BADID is a possibility, runs a special cycle to confirm.
            elsif Class = Identifier then
              Ada.Strings.Unbounded.Set_Unbounded_String(prevString,
              Ada.Strings.Unbounded.To_String(currString));
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
              lookAhead(iFile, tableHolder, table, Char, Class, prevClass,
              symClass, wasSym, Line, currString, prevString, echo);
            else  --No BADID, prints as is.
              Ada.Strings.Unbounded.Set_Unbounded_String(currString, "");--Reset
              outputFormat(Class, tableHolder, table, Line, currString);
              outputFormat(symClass, tableHolder, table, Line, currString);
            end if;
          end if;

        else  --Implies the Char wasn't a space or symbol,
          Ada.Strings.Unbounded.Append(currString, Char); --Add it to the string
        end if;
      end if;

    end loop;   --End of the line.
    if Ada.Strings.Unbounded.To_String(currString) /= "" then
      determineGrammar(Class, currString);
      outputFormat(Class, tableHolder, table, Line, currString);
      Ada.Strings.Unbounded.Set_Unbounded_String(currString, ""); --Reset
    end if;
    Skip_Line(iFile);  --Pass EOL buffer.
  end loop; --End of the file.
  New_Line; --Every new line, output buffer.
  New_Line; --Every new line, output buffer.
end fileRead;

end lex;

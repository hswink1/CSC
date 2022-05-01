--<****h*----
-- NAME
--  lex.ads - Scanner for given language.
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
-------->****

with Ada.Text_Io;	use Ada;
with Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling;
with objectpkg; use objectpkg;
use Text_IO;

package lex is

   --<****f* lex/isNumber
   -- DESCRIPTION
   --   Determines if every character in the string is a number.
   -- PARAMETERS
   --   convertedString - INPUT - string of possible number sequence.
   -- Return: Boolean - true if all characters are a number.
   -->****
   function isNumber(convertedString : in String) return Boolean;

   --<****f* lex/badNumCheck
   -- DESCRIPTION
   --   Determines if a string is a number sequence or error.
   -- PARAMETERS
   --   convertedString : String; - string of possible number sequence.
   -- RETURN
   --   Boolean - true if it is an error.
   -->****
   function badNumCheck(convertedString : in String) return Boolean;

   --<****f* lex/determineGrammar
   -- DESCRIPTION
   --   Determines the lexeme of a string.
   -- PARAMETERS
   --   Class : Char_Type; - lexeme of the string.
   --   currString : Unbounded_String; - unbounded string of characters.
   -->****
   procedure determineGrammar(Class : out Char_Type;
   currString : in Ada.Strings.Unbounded.Unbounded_String);

   --<****f* lex/determineSym
   -- DESCRIPTION
   --   Determines the lexeme of a symbol
   -- PARAMETERS
   --   Char : Character; - holder for possible symbol.
   --   symClass : Char_Type; - lexeme of symbol.
   --   wasSym : Boolean; - flag to determine if it was a symbol or not. Could
   --   not use OUT parameters in a function vs. procedure.
   --   Line : integer; - Used to append current line into object.
   -->****
   procedure determineSym(Char : in Character; symClass : out Char_Type;
   wasSym : out Boolean);

   --<****f* lex/outputFormat
   -- DESCRIPTION
   --   Adds the lexeme to a vector object.
   -- PARAMETERS
   --   Class : Char_Type; - Lexeme of the token.
   --   tableHolder : objectpkg.lexObject; - The object to be appended.
   --   table : objectpkg.lexTable;	- Vector of lexemes to be appended to.
   --   currString : Unbounded_String; - Token of lexeme
   -->****
   procedure outputFormat(Class : in Char_Type; tableHolder : in out objectpkg.lexObject;
   table : in out objectpkg.lexTable; Line : in Integer;
   currString : in Ada.Strings.Unbounded.Unbounded_String);

   --<****f* lex/lookAhead
   -- DESCRIPTION
   --   Reads the iFile and creates approriate output for oFile.
   -- PARAMETERS
   --   iFile : File_Type; - the file to be read.
   --   tableHolder : objectpkg.lexObject; - The object to be appended.
   --   table : objectpkg.lexTable;	- Vector of lexemes to be appended to.
   --   oFile : File_Type; - File to be outputted to.
   --   Char : Character; - holder for obtained character.
   --   Class : Char_Type; - lexeme of the string.
   --   prevClass : Char_Type; - lexeme of previous string.
   --   symClass : Char_Type; - lexeme of a symbol.
   --   wasSym : boolean; - Flag to remember if lexeme was a symbol.
   --   Line : integer; - Used to append current line into object.
   --   currString : Unbounded_String; - unbounded string to add to.
   --   prevString : Unbounded_String; - unbounded string to remember old string.
   --   echo : boolean;	- Holds a flag for raw input file text output.
   --
   -- In essence, copies the cycle and variables of fileRead, but alters output.
   -->****
   procedure lookAhead(iFile : in File_Type; tableHolder : in out objectpkg.lexObject;
   table : in out objectpkg.lexTable; Char : in out Character; Class : in out Char_Type;
   prevClass : in out Char_Type; symClass : in out Char_Type;
   wasSym : in out Boolean; Line : in out Integer;
   currString : in out Ada.Strings.Unbounded.Unbounded_String;
   prevString : in out Ada.Strings.Unbounded.Unbounded_String; echo : in boolean);

   --<****f* lex/fileRead
   -- DESCRIPTION
   --   Reads the iFile and creates approriate output for oFile.
   -- PARAMETERS
   --   iFile - INPUT - the file to be read.
   -- table : objectpkg.lexTable;	- Vector of lexemes to be appended to.
   -- echo : boolean;	- Holds a flag for raw input file text output.
   -->****
   procedure fileRead(iFile : in File_Type; table : in out objectpkg.lexTable;
   echo : in boolean);

end lex;

--<****h*-----------------------------------------------------------------------
-- NAME
--  objectpkg.ads - Package for object types.
-- AUTHOR
--  Henry Winkleman
-- CREATION DATE
--  16 April 2020
-- DESCRIPTION
--  The two objects in the package are built to hold a lexeme, token for
--  identifiers, and line number for those lexemes.
-- NOTES
--  Robodoc http://acad.kutztown.edu/~hwink972/csc310/project3.html
--------------------------------------------------------------------------->****

with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_Io;	use Ada;
with Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling;
use Text_IO;

package objectpkg is

  --<****t* objectpkg/String
  --DESCRIPTION
  -- Type is a 6 index array of unbounded strings.
  -->****
  type StringArray is array(1..6) of Ada.Strings.Unbounded.Unbounded_String;

  --<****t* objectpkg/Char_Type
  --DESCRIPTION
  -- Holds all of the available lexemes in the language, used for comparison.
  -->****
  type Char_Type is (Progsym, Decsym, Beginsym, Endsym, Typesym, Readsym,
  Writesym, Number, Identifier, Equals, Plus, Minus, Multiply, Divide, Colon,
  Semicolon, LParenthesis, RParenthesis, Comma, BADID, BADSYM, BADNUM, Blank);

  package Class_IO is new Ada.Text_IO.Enumeration_IO(Char_Type);
  use Class_IO;

  --<****t* objectpkg/lexObject
  --DESCRIPTION
   -- Holds:
   --   Class : Char_Type; - The lexeme of the object.
   --   Line : Integer; - The line number of the object.
   --   ID : Unbounded_String; - The token of the identifier
   -->****
  type lexObject is record
    Class : Char_Type;
    Line : Integer;
    ID : Ada.Strings.Unbounded.Unbounded_String;
  end record;

  package lexVector is new Ada.Containers.Vectors
    (Index_Type   => Natural,
     Element_Type => lexObject);

 --<****t* objectpkg/lexTable
 --DESCRIPTION
  -- Holds:
  --   A vector of lex objects.
  -->****
  type lexTable is record
    lexTbl : lexVector.vector;
  end record;

--<****t* objectpkg/idObject
--DESCRIPTION
 -- Holds:
 --   Line : Integer; - The line number of the object.
 --   ID : Unbounded_String; - The token of the used identifier
 -->****
  type idObject is record
    Line : Integer;
    ID : Ada.Strings.Unbounded.Unbounded_String;
  end record;

  package idVector is new Ada.Containers.Vectors
    (Index_Type   => Natural,
     Element_Type => idObject);

 --<****t* objectpkg/idTable
 --DESCRIPTION
  -- Holds:
  --   A vector of used ID objects.
  -->****
  type idTable is record
    idTbl : idVector.vector;
  end record;



end objectpkg;

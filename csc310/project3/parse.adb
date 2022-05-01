--<****h*-----------------------------------------------------------------------
-- NAME
--  parse.adb - Parser for given regular language.
-- AUTHOR
--  Henry Winkleman
-- CREATION DATE
--  16 April 2020
-- EXAMPLE
--  Enter a file that abides by the regular language given. The file is broken
--  down into lexemes and any errors with the syntax are reported along with the
--  location. Optional command line arguments /L /S and /E exist.
-- DESCRIPTION
--  The parser is responsible for testing the legality of inputted code,
--  reporting errors as necessary, but still attempting to run on certain
--  assumptions of human error in order to achieve accuracy and flexibility
--  in reporting.
-- NOTES
--  Robodoc http://acad.kutztown.edu/~hwink972/csc310/project3.html
-->**** 

with Ada.Text_Io;	use Ada;
with Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling;
with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Vectors;
with lex; use lex;
with objectpkg; use objectpkg;
use Text_IO;

procedure parse is

--<****f* parse/introduction
-- DESCRIPTION
--    Informs the user of the general design of the program.
-- PARAMETERS
--    N/A
-->****
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
  Put("mini grammar in MiniGrammar.pdf. Any errors will be");
  New_Line;
  Put("determined and outputted through parsing. Command");
  New_Line;
  Put("line arguments: /l <file> - Outputs lexemes to a");
  New_Line;
  Put("file, /s <file> - Outputs IDs to a file, /e - outputs");
  New_Line;
  Put("the file as it is read. Input file is also available");
  New_Line;
  Put("on the command line.");
  New_Line;
  Put("-------------------------------------------------------");
 	New_Line;
  New_Line;
 end introduction;

--<****f* parse/openIFile
--  DESCRIPTION
--    Opens user specified input file.
--  PARAMETERS
--    iFile : File_Type; - filestream for input.
--  NOTES
--    Credit to Dr. Spiegel,
--    https://faculty.kutztown.edu/spiegel/CSc310/Programs/Project2/OpenFile.adb.txt
-->****
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

--<****f* parse/getNext
--  DESCRIPTION
--    Grabs the next symbol from the symbol table.
--  PARAMETERS
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    errorCount : integer; - Holds the amount of errors encountered.
-->****
procedure getNext(symbol : in out objectpkg.lexObject; table : in out objectpkg.lexTable;
cursor : in out lexVector.cursor; errorCount : in out integer) is
  isBad : boolean := true;  --Continues loop upon a bad lexeme.
begin
  --Skips BAD SYM lexeme.
  while lexVector.Has_Element(cursor) and isBad loop
    lexVector.Next(cursor); --Gets next object.
    symbol := lexVector.Element(cursor);  --Sets object to cursor.
    isBad := false; --Assumes it isn't a bad lexeme.
    if symbol.Class = BADID then  --If BAD ID
      Put_Line("Found BAD ID at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
      --Changes symbol to identifier for ability to recover when checking for id
      symbol.Class := Identifier;
      lexVector.Replace_Element(table.lexTbl, cursor, symbol);
    elsif symbol.Class = BADSYM then  --If BAD SYMBOL
      Put_Line("Found BAD SYMBOL at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
      isBad := true;  --Continues loop
    elsif symbol.Class = BADNUM then  --If BAD NUMBER
      Put_Line("Found BAD NUMBER at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
      --Changes symbol to identifier for ability to recover when checking for num
      symbol.Class := Number;
      lexVector.Replace_Element(table.lexTbl, cursor, symbol);
    end if;
  end loop;
end getNext;

--<****f* parse/expectObject
--  DESCRIPTION
--    Grabs the next symbol from the symbol table.
--  PARAMETERS
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    errorCount : integer; - Holds the amount of errors encountered.
-->****
procedure expectObject(symbol : in out objectpkg.lexObject; table : in out objectpkg.lexTable;
cursor : in out lexVector.cursor; expectation : in Char_Type; found : in out boolean;
errorCount : in out integer) is
begin
  found := false; --Assumes expectation is not met.
  if symbol.Class = Endsym then --If the symbol held is End
    if symbol.Class = expectation then
      found := true;  --Return true if that was the expectation.
    end if;
  else
    for idx in 1..2 loop --Checks the currently held symbol and next symbol
      if symbol.Class = expectation then
        found := true;  --Return true if that was the expectation.
        if idx = 2  then  --If expectation was met in the 2nd symbol,
          lexVector.Previous(cursor); --Move cursor back
          symbol := lexVector.Element(cursor);  --Set symbol to cursor
          lexVector.Next(cursor); --Move cursor forward
          Put("Unexpected "); Class_IO.Put(symbol.Class);
          Put(" at line"); Put(Integer'Image(symbol.Line));
          Put(" when looking for "); Class_IO.Put(expectation);
          errorCount := errorCount + 1;
          New_Line;
        end if;
      else
        if idx /= 2 then
          getNext(symbol, table, cursor, errorCount); --Get the next symbol
        end if;
      end if;
      exit when found = true; --Exits loop if found prematurely.
    end loop;
    if found = false then --If no match was found,
      lexVector.Previous(cursor);
      symbol := lexVector.Element(cursor);  --Set cursor to original
    end if;
  end if;
end expectObject;

--<****f* parse/duplicateCheck
--  DESCRIPTION
--    Evaluates an object tagged as an ID to see if it has been declared or
--    referenced somewhere else in the file.
--  PARAMETERS
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    identifierTable : objectpkg.idTable;	- List of all used IDs.
--    decFlag : boolean; - Lets the procedure know if it should report an error
--    for duplicates.
--    errorCount : integer; - Holds the amount of errors encountered.
--    result : boolean; - Holds the result of the duplicate test.
-->****
procedure duplicateCheck(symbol : in objectpkg.lexObject;
identifierTable : in objectpkg.idTable; decFlag : in boolean;
errorCount : in out integer; result : in out boolean) is
  dupString : objectpkg.idObject; --Holds a string from the used list to compare.
  error : Boolean := false; --Ends loop if it hits a duplicate.
  cursor : idVector.cursor; --Used to iterate through the idTable.
begin
  cursor := idVector.First(identifierTable.idTbl);  --cursor points at first ID.
  --Runs while all elements have not been tested in the idTable.
  while idVector.Has_Element(cursor) and not error loop
    dupString := idVector.Element(cursor);  --Sets object to the cursor.
    if Ada.Strings.Unbounded.To_String(symbol.ID) = --If duplicates
    Ada.Strings.Unbounded.To_String(dupString.ID) then
      if decFlag then --If it is scanning for declarations
        Put_Line("Found DUPLICATE IDENTIFIER at line " & Integer'Image(symbol.Line));
        Put_Line("Found ORIGINAL IDENTIFIER at line " & Integer'Image(dupString.Line));
        errorCount := errorCount + 1;
      end if;
      error := true;  --Double declaration error.
      result := true; --Found duplicate.
    end if;
    idVector.Next(cursor);  --Check next used ID
  end loop;
  result := false;  --Did not find duplicate.
end duplicateCheck;

--<****f* parse/listVar
--  DESCRIPTION
--    Reads the symbols for a ID then acts recursively on a Comma.
--  PARAMETERS
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    identifierTable : objectpkg.idTable; - Vector holding all used IDs.
--    idHolder : idObject; - Holder for an used ID for comparison.
--    errorCount : integer; - Holds the amount of errors encountered.
--    startParse : boolean; - Changes error report based on if it is used
--    in dec part or not.
-->****
procedure listVar(table : in out objectpkg.lexTable; cursor : in out lexVector.cursor;
symbol : in out objectpkg.lexObject; identifierTable : in out objectpkg.idTable;
idHolder : in out idObject; errorCount : in out integer; startParse : in boolean) is
  expectation : Char_Type := Blank;
  found, duplicate, result : boolean := false;
begin
  --Look for ID
  expectObject(symbol, table, cursor, Identifier, found, errorCount);
  if found then
    --Duplicate check for case of double declaration in dec part.
    if startParse then
      duplicateCheck(symbol, identifierTable, true, errorCount, result);
      if result then  --Add ID to ID list.
        idHolder.ID := symbol.ID;
        idHolder.Line := symbol.Line;
        identifierTable.idTbl.Append(idHolder);
      end if;
    else  --Duplicate check for case of undeclared variable in statement part.
      duplicateCheck(symbol, identifierTable, false, errorCount, result);
      if result then
        Put_Line("Expected declared IDENTIFIER at line "
        & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
      end if;
    end if;
    --Look for Comma
    getNext(symbol, table, cursor, errorCount);
    expectObject(symbol, table, cursor, Comma, found, errorCount);
    if found then
      --Look for one or more IDs
      getNext(symbol, table, cursor, errorCount);
      listVar(table, cursor, symbol, identifierTable, idHolder, errorCount, startParse);
    else  --Assumes no comma, checks for ID in case of mistake.
      expectObject(symbol, table, cursor, Identifier, found, errorCount);
      if found then --Assumes forgotten comma
        Put_Line("Expected COMMA at line " & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
        --Restarts cycle in the case of more IDs
        getNext(symbol, table, cursor, errorCount);
        listVar(table, cursor, symbol, identifierTable, idHolder, errorCount, startParse);
      end if;
    end if;
  else  --Assumes no ID in list var.
    Put_Line("Expected IDENTIFIER at line "
    & Integer'Image(symbol.Line));
    errorCount := errorCount + 1;
  end if;
end listVar;

--<****f* parse/decPart
--  DESCRIPTION
--    Determines legality of declarations following dec.
--  PARAMETERS
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    identifierTable : objectpkg.idTable; - Vector holding all used IDs.
--    idHolder : idObject; - Holder for an used ID for comparison.
--    errorCount : integer; - Holds the amount of errors encountered.
-->****
procedure decPart(table : in out objectpkg.lexTable; cursor : in out lexVector.cursor;
symbol : in out objectpkg.lexObject; identifierTable : in out objectpkg.idTable;
idHolder : in out idObject; errorCount : in out integer) is
  found : boolean := false;
begin
  --Looks for one or more IDs
  listVar(table, cursor, symbol, identifierTable, idHolder, errorCount, true);
  --Looks for a following colon
  expectObject(symbol, table, cursor, Colon, found, errorCount);
  if found then
    --Looks for a data type
    getNext(symbol, table, cursor, errorCount);
    expectObject(symbol, table, cursor, Typesym, found, errorCount);
    if found then
      --End of declaration, gets next symbol.
      getNext(symbol, table, cursor, errorCount);
    else  --Assumes forgotten type
      Put_Line("Expected TYPE at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
    end if;
  else  --Assumes forgotten colon
    Put_Line("Expected COLON at line " & Integer'Image(symbol.Line));
    errorCount := errorCount + 1;
  end if;

  --Looks for semicolon for multiple declarations
  expectObject(symbol, table, cursor, Semicolon, found, errorCount);
  if found then
    --Restarts dec part if semicolon is found.
    getNext(symbol, table, cursor, errorCount);
    decPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
  --If an ID was found instead, assumes missing semicolon, restarts dec part.
  elsif symbol.Class = Identifier then
    Put_Line("Expected SEMICOLON at line " & Integer'Image(symbol.Line));
    errorCount := errorCount + 1;
    decPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
  else  --Assumes no following declaration, but isn't begin
    if symbol.Class /= Endsym and symbol.Class /= Beginsym then
      Put_Line("Expected SEMICOLON at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
    end if;
  end if;
end decPart;

--<****f* parse/inputOutput
--  DESCRIPTION
--    Determines legality of input/output statements.
--  PARAMETERS
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    identifierTable : objectpkg.idTable; - Vector holding all used IDs.
--    idHolder : idObject; - Holder for an used ID for comparison.
--    errorCount : integer; - Holds the amount of errors encountered.
-->****
procedure inputOutput(table : in out objectpkg.lexTable; cursor : in out lexVector.cursor;
symbol : in out objectpkg.lexObject; identifierTable : in out objectpkg.idTable;
idHolder : in out idObject; errorCount : in out integer) is
  expectation : Char_Type := Blank;
  found : boolean := false;
begin
  --Looks for left parenthesis.
  getNext(symbol, table, cursor, errorCount);
  expectObject(symbol, table, cursor, LParenthesis, found, errorCount);
  if found then
    --Looks for one or more IDs.
    getNext(symbol, table, cursor, errorCount);
    listVar(table, cursor, symbol, identifierTable, idHolder, errorCount, false);
    --Looks for resolving parenthesis.
    expectObject(symbol, table, cursor, RParenthesis, found, errorCount);
    if found then
      getNext(symbol, table, cursor, errorCount);
    else  --Assumes no parenthesis resolution.
      Put_Line("Expected RIGHT PARENTHESIS at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
    end if;
  else  --Assumes no parenthesis after Read/Write
    Put_Line("Expected LEFT PARENTHESIS at line " & Integer'Image(symbol.Line));
    errorCount := errorCount + 1;
  end if;
end inputOutput;

--<****f* parse/nestedParenthesis
--  DESCRIPTION
--    Determines legality of parenthesis inside of an expression.
--  PARAMETERS
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    errorCount : integer; - Holds the amount of errors encountered.
--    parenthesisCounter : integer; - Counts the unresolved parenthesis.
-->****
procedure nestedParenthesis(symbol : in out objectpkg.lexObject;
table : in out objectpkg.lexTable; cursor : in out lexVector.cursor;
errorCount : in out integer; parenthesisCounter : in out Integer) is
begin
    --Increases counter for unresolved left parenthesis
    if symbol.Class = LParenthesis then
      parenthesisCounter := parenthesisCounter + 1;
      getNext(symbol, table, cursor, errorCount);
      nestedParenthesis(symbol, table, cursor, errorCount, parenthesisCounter);
    --Decreases counter for resolved parenthesis
    elsif symbol.Class = RParenthesis then
      --If a right is found with no left, outputs error.
      if parenthesisCounter = 0 then
        Put_Line("Expected LEFT PARENTHESIS at line " & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
      else
        parenthesisCounter := parenthesisCounter - 1;
      end if;
      --Recursive call until no parenthesis is found
      getNext(symbol, table, cursor, errorCount);
      nestedParenthesis(symbol, table, cursor, errorCount, parenthesisCounter);
    end if;
end nestedParenthesis;

--<****f* parse/expression
--  DESCRIPTION
--    Determines legality of a mathematical expression following assignment.
--  PARAMETERS
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    identifierTable : objectpkg.idTable; - Vector holding all used IDs.
--    idHolder : idObject; - Holder for an used ID for comparison.
--    errorCount : integer; - Holds the amount of errors encountered.
--    parenthesisCounter : integer; - Counts the unresolved parenthesis.
-->****
procedure expression(table : in out objectpkg.lexTable; cursor : in out lexVector.cursor;
symbol : in out objectpkg.lexObject; identifierTable : in out objectpkg.idTable;
idHolder : in out idObject; errorCount : in out integer;
parenthesisCounter : in out Integer) is
  expectation : Char_Type := Blank;
  idFound, numFound, pFound, miFound, muFound, dFound, result : boolean := false;
begin
  --Tests for parenthesis, then all for possible math expressions.
  getNext(symbol, table, cursor, errorCount);
  nestedParenthesis(symbol, table, cursor, errorCount, parenthesisCounter);
  expectObject(symbol, table, cursor, Plus, pFound, errorCount);
  expectObject(symbol, table, cursor, Minus, miFound, errorCount);
  expectObject(symbol, table, cursor, Multiply, muFound, errorCount);
  expectObject(symbol, table, cursor, Divide, dFound, errorCount);
  --If any expression found,
  if pFound or miFound or muFound or dFound then
    --Test for parenthesis, then ID or number.
    getNext(symbol, table, cursor, errorCount);
    nestedParenthesis(symbol, table, cursor, errorCount, parenthesisCounter);
    expectObject(symbol, table, cursor, Identifier, idFound, errorCount);
    if idFound then
      duplicateCheck(symbol, identifierTable, false, errorCount, result);
      if result then
        Put_Line("Expected declared IDENTIFIER at line "
        & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
      end if;
    else
      expectObject(symbol, table, cursor, Number, numFound, errorCount);
    end if;
    --Recursive call for possibility of multiple expressions.
    if idFound or numFound then
      expression(table, cursor, symbol, identifierTable, idHolder, errorCount, parenthesisCounter);
    else  --Assumes no following ID to expression.
      Put_Line("Expected IDENTIFIER or NUMBER at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
    end if;
  else  --If any unresolved parenthesis, outputs error.
    if parenthesisCounter > 0 then
      Put_Line("Expected" & Integer'Image(parenthesisCounter) &
      " RIGHT PARENTHESIS at line" & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
    end if;
  end if;
end expression;

--<****f* parse/statementPart
--  DESCRIPTION
--    Determines legality of a statement following begin, acts recursively.
--  PARAMETERS
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
--    cursor : lexVector.cursor; - Iterates across the lexeme table.
--    symbol : objectpkg.lexObject; - The object to be evaluated for duplicates.
--    identifierTable : objectpkg.idTable; - Vector holding all used IDs.
--    idHolder : idObject; - Holder for an used ID for comparison.
--    errorCount : integer; - Holds the amount of errors encountered.
-->****
procedure statementPart(table : in out objectpkg.lexTable; cursor : in out lexVector.cursor;
symbol : in out objectpkg.lexObject; identifierTable : in out objectpkg.idTable;
idHolder : in out idObject; errorCount : in out integer) is
  expectation : Char_Type := Blank;
  found, secondFound, result : boolean := false;
  contProg : boolean := true;
  parenthesisCounter : Integer := 0;
begin

  if lexVector.Has_Element(cursor) then
    --Input or output statment.
    if symbol.Class = Readsym or symbol.Class = Writesym then
      inputOutput(table, cursor, symbol, identifierTable, idHolder, errorCount);

    --Expressions
    elsif symbol.Class = Identifier then
      --Look for equals
      getNext(symbol, table, cursor, errorCount);
      expectObject(symbol, table, cursor, Equals, found, errorCount);
      if found then
        --Look for ID or number
        getNext(symbol, table, cursor, errorCount);
        nestedParenthesis(symbol, table, cursor, errorCount, parenthesisCounter);
        expectObject(symbol, table, cursor, Identifier, found, errorCount);
        if found then
          duplicateCheck(symbol, identifierTable, false, errorCount, result);
          if result then
            Put_Line("Expected declared IDENTIFIER at line "
            & Integer'Image(symbol.Line));
            errorCount := errorCount + 1;
          end if;
        else
          expectObject(symbol, table, cursor, Number, secondFound, errorCount);
        end if;
        if found or secondFound then
          --Look for continued expression.
          expression(table, cursor, symbol, identifierTable, idHolder, errorCount, parenthesisCounter);
        else  --Assumes no following ID or number.
          Put_Line("Expected IDENTIFIER or NUMBER at line " & Integer'Image(symbol.Line));
          errorCount := errorCount + 1;
        end if;
      else  --Assumes not assignment.
        Put_Line("Expected EQUALS at line " & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
      end if;

    --If end is found
    elsif symbol.Class = Endsym then
      contProg := false;  --End recursion.
    else  --Assumes illegal grammar.
      Put_Line("Expected LEGAL FUNCTION at line " & Integer'Image(symbol.Line));
      Put("Found "); Class_IO.Put(symbol.Class); Put(" instead."); New_Line;
      errorCount := errorCount + 1;
      getNext(symbol, table, cursor, errorCount);
      --Recursion.
      statementPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
    end if;

    if contProg then  --If end has not been found,
      --Look for a semicolon.
      expectObject(symbol, table, cursor, Semicolon, found, errorCount);
      if found then
        --Start recursion.
        getNext(symbol, table, cursor, errorCount);
        statementPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
      --Looks for statements to be ended with semicolon if they are not the last.
      elsif symbol.Class = Readsym or symbol.Class = Writesym
      or symbol.Class = Identifier then
        Put_Line("Expected SEMICOLON at line " & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
        statementPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
      else
        if symbol.Class /= Endsym then
          Put_Line("Expected SEMICOLON at line " & Integer'Image(symbol.Line));
          errorCount := errorCount + 1;
          --Recursion.
          statementPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
        end if;
      end if;
    end if;
  end if;
end statementPart;

--<****f* parse/startParse
--  DESCRIPTION
--    Starts parsing, beginning with a expectation of "program".
--  PARAMETERS
--    table : objectpkg.lexTable;	- List of all lexemes extracted from the file.
-->****
procedure startParse(table : in out objectpkg.lexTable) is
  cursor : lexVector.cursor;    --Iterator for vector
  symbol : objectpkg.lexObject; --Holder for symbol
  identifierTable : objectpkg.idTable;  --Vector for used IDs
  idHolder : idObject;                  --Holder for used IDs

  previousLine, errorCount : Integer := 0;
  startParseFlag : Boolean := true;
  found, expectID : boolean := true;
  beginError : boolean := false;
  expectation : Char_Type := Blank;
begin
  --Sets the cursor to the beginning
  cursor := lexVector.First(table.lexTbl);
  if lexVector.Has_Element(cursor) then
    symbol := lexVector.Element(cursor);
    --Looks for program
    expectObject(symbol, table, cursor, Progsym, found, errorCount);
    if found then
      getNext(symbol, table, cursor, errorCount);
      --Looks for program ID
      expectObject(symbol, table, cursor, Identifier, found, errorCount);
      if found then --Adds it to the list as it is the first ID used.
        idHolder.ID := symbol.ID;
        idHolder.Line := symbol.Line;
        identifierTable.idTbl.Append(idHolder);
        getNext(symbol, table, cursor, errorCount);
      else  --Assumes no ID found.
        Put_Line("Expected IDENTIFIER at line " & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
      end if;
      --Looks for Dec
      expectObject(symbol, table, cursor, Decsym, found, errorCount);
      if found then
        --Starts dec part if found.
        getNext(symbol, table, cursor, errorCount);
        decPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
      else  --Assumes no dec
        Put_Line("Expected DEC at line " & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
      end if;
    else
      Put_Line("Expected PROGRAM at line " & Integer'Image(symbol.Line));
      errorCount := errorCount + 1;
    end if;
  end if;


  while startParseFlag and lexVector.Has_Element(cursor) loop
  --Looks for begin
    expectObject(symbol, table, cursor, Beginsym, found, errorCount);
    if found then
      startParseFlag := false;
      --Starts the beginning of the statement part.
      getNext(symbol, table, cursor, errorCount);
      statementPart(table, cursor, symbol, identifierTable, idHolder, errorCount);
    else  --Assumes symbol wasn't begin.
      if not beginError then  --Has error only display once.
        Put_Line("Expected BEGIN at line " & Integer'Image(symbol.Line));
        errorCount := errorCount + 1;
        beginError := true;
      end if;
      getNext(symbol, table, cursor, errorCount);
    end if;
  end loop;

  --If an error was found,
  if errorCount > 0 then
    --Output how many.
    Put_Line("Found" & Integer'Image(errorCount) & " errors.");
  else  --Assumes no errors.
    put_line("Success.");
  end if;
end startParse;

--<****f* parse/slashArg
--  DESCRIPTION
--    Determines if any argument is a slash command.
--  PARAMETERS
--    arg : Unbounded_String; - Holds the string for an argument.
-->****
function slashArg(arg : in Ada.Strings.Unbounded.Unbounded_String) return Integer is
  --GUIDE: 1 for INPUT FILENAME, 2 for LEXEME OUTPUT, 3 for ID OUTPUT, 4 for ECHO.
begin
  --Looks for lex output command.
  if Ada.Strings.Unbounded.To_String(arg) = "/l" or
  Ada.Strings.Unbounded.To_String(arg) = "/L" then
    return 2;
  --Looks for ID output command.
  elsif Ada.Strings.Unbounded.To_String(arg) = "/s" or
  Ada.Strings.Unbounded.To_String(arg) = "/S" then
    return 3;
  --Looks for echo command.
  elsif Ada.Strings.Unbounded.To_String(arg) = "/e" or
  Ada.Strings.Unbounded.To_String(arg) = "/E" then
    return 4;
  --Assumes argument is a filename.
  else
    return 1;
  end if;
end slashArg;

--<****f* parse/determineArgs
--  DESCRIPTION
--    Determines the type and function of arguments entered on the command line.
--  PARAMETERS
--    lexO : boolean;	- Holds a flag for lexeme output.
--    symO : boolean;	- Holds a flag for symbol output.
--    echo : boolean;	- Holds a flag for raw input file text output.
--    commandError : boolean;	- Holds a flag for errors on the command line.
--    args : StringArray; - Holder for array type of argument strings.
--    fileNames : StringArray; - Holder for array type of filename arguments.
-->****
procedure determineArgs(lexO : in out boolean; symO : in out boolean;
echo : in out boolean; iFileFlag : in out boolean; commandError : in out boolean;
args : in out StringArray; fileNames : in out StringArray) is
  resultHolder : integer;
  skipFlag : boolean := false;
  --GUIDE: 1 for INPUT FILENAME, 2 for LEXEME OUTPUT, 3 for ID OUTPUT, 4 for ECHO.
begin
  --For all arguments
  for idx in 1..Argument_Count loop
    if skipFlag then  --Will skip next argument in some cases.
      skipFlag := false;
    else  --Assumes no skip.
      --Determines type of current argument.
      resultHolder := slashArg(args(idx));
      --Echo command.
      if resultHolder = 4 then
        echo := true;
      --ID output command.
      elsif resultHolder = 3 then
        symO := true;
        --Assumes next argument is the filename for output.
        if slashArg(args(idx+1)) = 1 then
          fileNames(3) := args(idx+1);
          skipFlag := true;
        else
          Put_Line("Command line error: Expected FILE following /S command.");
          commandError := true;
        end if;
      --Lex output command.
      elsif resultHolder = 2 then
        lexO := true;
        --Assumes next argument is the filename for output.
        if slashArg(args(idx+1)) = 1 then
          fileNames(2) := args(idx+1);
          skipFlag := true;
        else
          Put_Line("Command line error: Expected FILE following /L command.");
          commandError := true;
        end if;
      else  --Implies it isn't a slash command.
        --If no input file has been determined,
        if not iFileFlag then
          --Argument becomes input file.
          fileNames(1) := args(idx);
          iFileFlag := true;
        else  --Assumes unnecessary filename.
          Put_Line("Command line error: Unexpected command or file name.");
          commandError := true;
        end if;
      end if;
    end if;
  end loop;
end determineArgs;

--------------------------------------------------------------------------
--------------------------------------------------------------------------
-----------------------------------MAIN-----------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------

  table : objectpkg.lexTable;   --Vector of symbol objects.
  cursor : lexVector.cursor;    --Iterator for vector.
  symbol : objectpkg.lexObject; --Holder for symbols.

  iFileFlag, lexO, symO, echo, commandError : boolean := false;
  args, fileNames : StringArray;
  lexOFile, symOFile, iFile : File_Type;
  prevLine : integer := 1;
  --GUIDE: 1 for INPUT FILENAME, 2 for LEXEME OUTPUT, 3 for ID OUTPUT, 4 for ECHO.

begin --Start main.
  introduction; --Explains program.
  if Argument_Count > 0 then --If there is a command line argument,
    for idx in 1..Argument_Count loop --For all arguments,
      --Add them to an array.
      Ada.Strings.Unbounded.Set_Unbounded_String(args(idx), Argument(idx));
    end loop;
    --Sort the arguments and commands.
    determineArgs(lexO, symO, echo, iFileFlag, commandError, args, fileNames);
    if not commandError then  --If there was no command error.

      if iFileFlag then --If input file was specified
        --Starts scanning input file.
        open(File=>iFile, Mode=>in_file,
        Name=>Ada.Strings.Unbounded.To_String(fileNames(1))); --Opens file
        fileRead(iFile, table, echo); --Starts scanning input file.
        Close(iFile);
      else
        --Starts scanning input file.
        openIFile(iFile);
        fileRead(iFile, table, echo); --Starts scanning input file.
        Close(iFile);
      end if;

      if lexO then  --If lexeme output,
        --Generate specified file.
        Create(File=>lexOFile, Mode=>out_file,
        Name=>Ada.Strings.Unbounded.To_String(fileNames(2)));
        cursor := lexVector.First(table.lexTbl);
        --For all lexemes, print them as they were read.
        while lexVector.Has_Element(cursor) loop
          symbol := lexVector.Element(cursor);
          --If it is a newline, print old line.
          if symbol.Line /= prevLine then
            New_Line(lexOFile);
          end if;
          Class_IO.Put(lexOFile, symbol.Class);
          Put(lexOFile, ' ');
          prevLine := symbol.Line;
          lexVector.Next(cursor);
        end loop;
        Close(lexOFile);
      end if;
    end if;


    if symO then  --If ID output,
      --Create output file.
      Create(File=>symOFile, Mode=>out_file,
      Name=>Ada.Strings.Unbounded.To_String(fileNames(3)));
      cursor := lexVector.First(table.lexTbl);
      --For all lexemes that classify as IDs,
      while lexVector.Has_Element(cursor) loop
        symbol := lexVector.Element(cursor);
        if symbol.Class = Identifier then
          --Print as the identifier and the line number.
          Put(symOFile, Integer'Image(symbol.Line));
          Put(symOFile, ' ');
          Put(symOFile, Ada.Strings.Unbounded.To_String(symbol.ID));
          New_Line(symOFile);
        end if;
        lexVector.Next(cursor);
      end loop;
      Close(symOFile);
    end if;

    startParse(table);



  else  --Assumes no command line arguments.
    openIFile(iFile);
    fileRead(iFile, table, echo); --Starts scanning input file.
    Close(iFile);
    startParse(table);
  end if;
end parse;

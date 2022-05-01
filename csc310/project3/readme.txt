Design:
	The core to this program is based on the lexeme evaluation in the scanner being
saved to a vector, along with the line number and string (although the string in some cases
is not actually representative of what was read during scanning). By having the lexemes
already evaluated, I merely needed to compare the Char_Type of the object with my
expectations instead of re-evaluation.
	As for expectations, my parser is made in a predictive fashion. In essence, since
I know the possible legal options of the minigrammar, then I am able to predict what lexeme
should come next as there are a finite amount of legal options. My expectObject function
is the crux of this as it compares the currently held lexeme with the expected lexeme,
even looking ahead one space in the case of a misinput or unexpected lexeme. This allowed
me to repeat the looking ahead without hardcoding it into every expectation, but that
required a static number of cycles, as having a fluid number of cycles can eat lexemes
that were intended.
	Most of the project works in descent recursion as desired, listVar will repeat
on comma, statementPart on semicolon, etc..
	getNext is also another interesting function of my design. In essence, it will
iterate across the vector and grab the next symbol, but if the lexeme would be bad (BADID,
BADNUM, BADSYM) it displays an error at that moment instead of later on, and in the
case of BADSYM, grabs the next symbol. In the other two cases, it changes the symbol in
the table to be of Identifier or Number in order to recover in certain cases.
	I do not believe I have made any other wild choices aside from that.

Tables:
	I have two tables in my objectpkg, lexTable and idTable. The word "symbol" as used
recently and during the course of this project has been fairly ambiguous, it has been
used as an actual symbol (* & $), another name for a lexeme, or an identifier. I tried to
avoid the word symbol when it came to the tables in order to prevent the ambiguity, but
it still exists in my parser, which is perhaps annoying though deserved for not forseeing
that during early construction.

	lexTable:

	Lexeme Table is responsible for holding the lexeme, token, and line of the strings
extracted in the scanner. In essence, all of the data I would likely need from the scanner.

	idTable:

	Identifier Table is responsible for storing the token and line of an identifier
when declared, and thus allowing me to see if the call for a specific identifier is legal.

	They are both constructed by creating an object to hold the specified information
and then applying that as the element of a vector.


Non-Terminal Processes:
-> = found
| = not found

listVar: Predicts an ID - predicts a comma -> recursion.
			                   |Ends prediction    |Looks for ID -> recursion
							                                               |Ends prediction

decPart: listvar then
predictions for colon and type -> prediction for semicolon -> recursion
				                        |Ends prediction    	   |Looks for ID -> recursion
							                                                           |Ends prediction

statementPart: Very similar to decPart
predictions for statement -> prediction for semicolon -> recursion
			                     |Ends prediction    	   	|Looks for ID -> recursion
							                                                     |Ends prediction

nestedParenthesis: covers factor case
predictions for parenthesis -> recursion
			                       |Ends prediction, assumes ID or Num

expression: Very similar to decPart
At any time getNext would happen nestedParenthesis happens
prediction for id/num -> prediction for +-*/ -> recursion
			                 |Ends prediction      |ends prediction


Any others like input or output are a static set of predictions that eventually would
call on a separate non-terminal like listVar.


Known bugs:
There is no check for BAD ID, BAD SYM, BAD NUM for the first symbol.

There is an automatic crash if more than 6 command line arguments are entered as it
breaks the array that is supposed to hold them.

The "Expected LEGAL FUNCTION" error in statement part will loop until Read, Write, or an
ID is found.

There is no error for an unnecessary semicolon before end in the statement part.



Running the program:

	I created an introduction that lists the command line arguments after launching the
program. You will be able to launch without arguments and it will prompt you for an input
file that has been written in minigrammar. Aside from that, it will take /l, /s, /e, and
a separate input filename in no particular order, except for requiring a filename to
follow /l and /s for the output file. This just means that if you had /l open.txt test.txt,
the first file, open.txt, is recognized as the symbol output file, the second being the
input file. It should only require four files to compile, parse.adb, objectpkg.ads, lex.adb,
and lex.ads.

To note: Errors that involve expectations for semicolons will read as being expected one
line ahead of the line it would normally be written on. This is for two reasons: it is not
illegal to place the semicolon on the newline, it would just need to be present in the first
place. It is also easier to output the error line this way as I do not need to retain info
for the previous symbol that would have existed on the previous line.

Robodoc singledoc: http://acad.kutztown.edu/~hwink972/csc310/project3.html

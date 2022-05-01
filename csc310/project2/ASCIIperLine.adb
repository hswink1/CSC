-- File: ASCIIperLine.adb
--	Add the ASCII values on each line of text.
--	Output the results, line by line, and an overall total at the end.
--	Demo of:
--	 Command line arguments
--	 file access
--	 Character input
--	 End of line (EOLN)
--	 Output of enumerated elements
--
-- Note: Text file must end with carriage return for this to work

with Ada.Text_Io;	use Ada;
with Ada.Integer_Text_IO;
with OpenFile;		use OpenFile;
with Ada.Command_Line;  use Ada.Command_Line;

procedure ASCIIperCmdLine is

   type Char_Type is (Alpha, Num, Op, Blank, X);

   package Class_IO is new Ada.Text_IO.Enumeration_IO(Char_Type);
   use Class_IO;

   Class : 			Char_Type;
   InFile,OutFile,EnumFile: 	Text_IO.File_Type;
   Char: 			Character;
   SumLine,Total:               Integer;

begin
   Text_IO.Put_Line("Form: ASCIIperCmdLine <Source> <Dest> <Enum Values>");
   if Argument_Count = 0 Then
      OpenReadFile(InFile);		-- Get file to process
      Text_IO.put("Totals Destination:"); Text_IO.New_Line;
      OpenWriteFile(OutFile);		-- Get file for numeric results
      Text_IO.put("Enum Values Destination:"); Text_IO.New_Line;
      OpenWriteFile(Enumfile);		-- Get file for Symbolic results
   ElsIf Argument_Count = 3 Then
      Text_IO.open(File=>InFile, Mode=>Text_IO.in_file, Name=>Argument(1));
      Text_IO.Create(File=>OutFile, Mode=>Text_IO.out_file, Name=>Argument(2));
      Text_IO.Create(File=>EnumFile, Mode=>Text_IO.out_file, Name=>Argument(3));
   End If;
   Total:=0;
   while not Text_Io.End_Of_File (Infile) loop
      -- Note: Text file must end with CR for this to work
      SumLine:=0;
      while not Text_IO.End_Of_Line (InFile) loop
         Text_IO.Get (File=>InFile, Item=>Char);
	   Text_IO.Put(Char);

         case Char is
            when 'A' .. 'Z' | 'a' .. 'z' =>
               Class := Alpha;
            when '0' .. '9' =>
               Class := Num;
            when '*' | '+' | '-' | '/' | '<' .. '>' =>
               Class := Op;
            when ' ' =>
               Class:=Blank;
            when others =>
               Class := X;
         end case;
         SumLine:=SumLine+Character'Pos(Char);
         -- output to enum file;
	 Class_IO.Put(Enumfile,Class); Text_Io.Put(Enumfile,' ');
	             -- why Text_IO in one only? Is Class_IO necessary?
      end loop;
      Integer_Text_IO.put(OutFile,SumLine);
      Text_IO.New_Line (Outfile);  -- Move pointer in OutFile to new line
      Text_IO.Skip_Line (InFile);  -- Move pointer in input buffer past EOL
      Text_IO.New_Line;
      Text_IO.New_Line (Enumfile); -- Move pointer in EnumFile to new line
      Total:=Total+SumLine;
   end loop;
   Integer_Text_IO.Put(OutFile,Total);
   Text_IO.New_Line (Outfile);
   Text_IO.Close(Infile);
   Text_IO.Close(Outfile);
   Text_IO.Close(EnumFile);
end ASCIIperCmdLine;

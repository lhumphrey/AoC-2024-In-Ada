with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with GNAT.Regpat; use GNAT.Regpat;

procedure Main is

   type Token_Type is (On, Off, Mul, EOL);
   Current_Token : Token_Type;

   Re_Mul : constant Pattern_Matcher := Compile ("mul\((\d+),(\d+)\)");
   Mul_Matches : Match_Array (0 .. 2);
   D1, D2 : Natural;

   Re_Switch : constant Pattern_Matcher := Compile ("do\(\)|don't\(\)");
   Switch_Matches : Match_Array (0 .. 0);
   Do_Status : Boolean := True;

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 4096);
   Idx, Last_Idx : Natural;
   Total1, Total2 : Natural := 0;

   procedure Process_Next_Token is
   begin
      Match (Re_Switch, S (Idx .. Last_Idx), Switch_Matches);
      Match (Re_Mul, S (Idx .. Last_Idx), Mul_Matches);

      if Mul_Matches (0) = No_Match then
         if Switch_Matches (0) = No_Match then
            Current_Token := EOL;
            Idx := Last_Idx;
         else
            Current_Token :=
              (if Switch_Matches (0).Last - Switch_Matches (0).First = 3
               then On else Off);
            Idx := Switch_Matches (0).Last + 1;
         end if;
      else
         if Switch_Matches (0) = No_Match or else
           Mul_Matches (0).First < Switch_Matches (0).First
         then
            Current_Token := Mul;
            D1 := Natural'Value (S (Mul_Matches (1).First .. Mul_Matches (1).Last));
            D2 := Natural'Value (S (Mul_Matches (2).First .. Mul_Matches (2).Last));
            Idx := Mul_Matches (0).Last + 1;
         else
            Current_Token :=
              (if Switch_Matches (0).Last - Switch_Matches (0).First = 3
               then On else Off);
            Idx := Switch_Matches (0).Last + 1;
         end if;
      end if;
   end Process_Next_Token;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, Last_Idx);
      Idx := 1;
      while Idx < Last_Idx loop
         Process_Next_Token;
         case Current_Token is
            when Mul =>
               Total1 := Total1 + (D1 * D2);
               if Do_Status then
                  Total2 := Total2 + (D1 * D2);
               end if;
            when On =>
               Do_Status := True;
            when Off =>
               Do_Status := False;
            when others =>
               null;
         end case;
      end loop;
   end loop;

   Close (File);

   Put_Line("Part 1 total: " & Total1'Image);
   Put_Line("Part 2 total: " & Total2'Image);

end;

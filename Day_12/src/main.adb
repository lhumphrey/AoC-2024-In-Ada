with Ada.Text_IO; use Ada.Text_IO;
with Gardens;

procedure Main is
   Filename : String := "input.txt";
   File : File_Type;
   Rows, Cols : Natural := 0;
   S : String (1 .. 256);
   S_Last : Natural := 0;
   Padded_Input : array (1 .. 256, 1 .. 256) of Character;
begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, S_Last);
      if Cols = 0 then
         Cols := S_Last;
      elsif Cols /= S_Last then
         Put_Line ("Error: Input had inconsistent number of columns");
         return;
      end if;
      Rows := Rows + 1;
      for I in 1 .. Cols loop
         Padded_Input (Rows, I) := S (I);
      end loop;
   end loop;

   Close (File);

   declare
      Row : Positive := 1;
      Total_1, Total_2 : Natural := 0;
      package Garden is new Gardens (Rows, Cols);
      use Garden;
   begin
      for I in Puzzle'Range (1) loop
         for J in Puzzle'Range (2) loop
            Puzzle (I, J) := Padded_Input (I, J);
         end loop;
      end loop;

      Initialize_Garden;

      for E of Regions loop
         Total_1 := Total_1 + E.Perimeter * E.Area;
      end loop;
      Put_Line ("Total price based on perimeter and area: " & Total_1'Image);

      Update_Regions_With_Sides;

      for E of Regions loop
         Total_2 := Total_2 + E.Sides * E.Area;
      end loop;
      Put_Line ("Total price based on number of sides and area: " & Total_2'Image);
   end;

end Main;

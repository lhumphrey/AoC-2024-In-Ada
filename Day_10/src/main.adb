with Ada.Text_IO; use Ada.Text_IO;
with Trails; use Trails;

procedure Main is

   Filename : String := "input.txt";
   File : File_Type;
   Map_Input : array (1 .. 100, 1 .. 100) of Natural;
   Map_Height, Map_Width : Natural := 0;
   Total_1, Total_2 : Natural := 0;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      declare
         S : String (1 .. 100);
         S_Last : Natural;
      begin
         Map_Height := Map_Height + 1;
         Get_Line (File, S, S_Last);
         for I in 1 .. S_Last loop
            Map_Input (Map_Height, I) := Natural'Value (S (I .. I));
         end loop;
         Map_Width := (if S_Last > 1 then S_Last else Map_Height);
      end;
   end loop;

   Close (File);

   declare
      Map : Trail_Map (1 .. Map_Height, 1 .. Map_Width);
   begin
      for Row in Map'Range (1) loop
         for Col in Map'Range (2) loop
            declare
               G : Goal := Goal'(Row, Col);
               L : Location := Location'(Map_Input (Row, Col), Goal_Vectors.Empty);
            begin
               if L.Height = 9 then
                  L.Reachable_Goals.Append (G);
                  Path_Id := Path_Id + 1;
               end if;
               Map (Row, Col) := L;
            end;
         end loop;
      end loop;

      for Row in Map'Range (1) loop
         for Col in Map'Range (2) loop
            Descend (Map, Row, Col);
         end loop;
      end loop;

      for Row in Map'Range (1) loop
         for Col in Map'Range (2) loop
            if Map (Row, Col).Height = 0 then
               Total_1 := Total_1 + Integer (Map (Row, Col).Reachable_Goals.Length);
            end if;
         end loop;
      end loop;

      Put_Line ("Total 1 is: " & Total_1'Image);

      for Row in Map'Range (1) loop
         for Col in Map'Range (2) loop
            if Map (Row, Col).Height = 0 then
               Total_2 := Total_2 + Ascend (Map, Row, Col);
            end if;
         end loop;
      end loop;

      Put_Line ("Total 2 is: " & Total_2'Image);
   end;

end Main;

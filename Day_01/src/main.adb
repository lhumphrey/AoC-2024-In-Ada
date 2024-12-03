with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main with SPARK_Mode is

   package Natural_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Natural);
   use Natural_Vectors;

   package Natural_Vectors_Sorting is new
     Natural_Vectors.Generic_Sorting;
   use Natural_Vectors_Sorting;

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 256);
   Last_Idx, Idx : Natural;
   Num, Total_Dist, Sim_Score : Natural := 0;
   Vec1, Vec2 : Vector;
begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, Last_Idx);

      if Last_Idx > 0 then
         Get (S (1 .. Last_Idx), Num, Idx);
         Vec1.Append (Num);
         Get (S (Idx + 1 .. Last_Idx), Num, Idx);
         Vec2.Append (Num);
      end if;

   end loop;

   Close (File);

   Sort (Vec1);
   Sort (Vec2);

   for I in First_Index (Vec1) .. Last_Index (Vec1) loop
      Total_Dist := Total_Dist + abs (Vec1.Element (I) - Vec2.Element (I));
   end loop;

   Put_Line ("Part 1 total distance: " & Total_Dist'Image);

   for E of Vec1 loop
      Idx := Find_Index (Vec2, E);
      if Idx > 0 then
         declare
            Count : Natural := 1;
         begin
            Idx := Idx + 1;
            while Idx <= Last_Index (Vec2) and Vec2.Element (Idx) = E loop
               Count := Count + 1;
               Idx := Idx + 1;
            end loop;
            Sim_Score := Sim_Score + (E * Count);
         end;
      end if;
   end loop;

   Put_Line ("Part 2 similarity score: " & Sim_Score'Image);

end;

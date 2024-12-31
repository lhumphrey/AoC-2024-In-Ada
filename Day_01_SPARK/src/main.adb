with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with SPARK.Containers.Formal.Vectors;
with Ada.Containers; use Ada.Containers;

procedure Main with SPARK_Mode, Always_Terminates => False is

   subtype Input is Integer range 1 .. 99999;

   package Input_Vectors is new
     SPARK.Containers.Formal.Vectors (Positive, Input);
   use Input_Vectors;

   package Input_Vectors_Sorting is new Input_Vectors.Generic_Sorting;
   use Input_Vectors_Sorting;

   Filename : constant String := "input.txt";
   File : File_Type;
   S : String (1 .. 32) with Relaxed_Initialization;
   Last_Idx, Idx : Natural;
   Num : Integer;
   Total_Dist, Sim_Score : Integer := 0;
   Vec1, Vec2 : Vector (1501);
begin

   begin
      Open (File, In_File, Filename);
   exception
      when others =>
         Put_Line ("Error: Could not open file.");
         return;
   end;

   while not End_Of_File (File) loop
      begin
         Get_Line (File, S, Last_Idx);
      exception
         when others =>
            Put_Line ("Error: Could not read line from file.");
            return;
      end;
      if Last_Idx > 0 then
         begin
            Get (S (1 .. Last_Idx), Num, Idx);
         exception
            when others =>
               Put_Line ("Error: Could not convert string input to number.");
               return;
         end;
         if Num not in Input'Range then
            Put_Line ("Error: Input number greater than expected.");
            return;
         end if;
         if Length (Vec1) < Vec1.Capacity then
            Append (Vec1, Num);
         else
            Put_Line ("Error: Capacity of Vec1 would have been exceeded.");
            return;
         end if;
         if Idx < Natural'Last then
            Idx := Idx + 1;
         else
            Put_Line ("Error: Idx would have overflowed.");
            return;
         end if;
         begin
            Get (S (Idx .. Last_Idx), Num, Idx);
         exception
            when others =>
               Put_Line ("Error: Could not convert string input to number.");
               return;
         end;
         if Length (Vec2) < Vec2.Capacity and then Num in Input'Range then
            Append (Vec2, Num);
         else
            Put_Line ("Error: Capacity of Vec2 would have been exceeded.");
            return;
         end if;
      end if;
      pragma Loop_Invariant (Last_Index (Vec1) = Last_Index (Vec2));
   end loop;

   Close (File);

   Sort (Vec1);
   Sort (Vec2);

   for I in First_Index (Vec1) .. Last_Index (Vec1) loop
      pragma Loop_Invariant (Last_Index (Vec1) = Last_Index (Vec2));
      if Total_Dist < Integer'Last - abs (Element (Vec1, I) - Element (Vec2, I))
      then
         Total_Dist := Total_Dist + abs (Element (Vec1, I) - Element (Vec2, I));
      else
         Put_Line ("Error: Total_Dist would have overflowed.");
         return;
      end if;
   end loop;

   Put_Line ("Part 1 total distance: " & Total_Dist'Image);

   for E of Vec1 loop
      Idx := Find_Index (Vec2, E);
      if Idx > 0 then
         declare
            Count : Natural := 0;
         begin
            pragma Assert
              (Last_Index (Vec2) <= Integer (Capacity (Vec2)));
            while Idx <= Last_Index (Vec2) and then Element (Vec2, Idx) = E loop
               Count := Count + 1;
               Idx := Idx + 1;
               pragma Loop_Invariant (Count = Idx - Idx'Loop_Entry);
               pragma Loop_Invariant (Idx - 1 <= Last_Index (Vec2));
            end loop;
            if Sim_Score <= Integer'Last - (E * Count) then
               Sim_Score := Sim_Score + (E * Count);
            else
               Put_Line ("Error: Sim_Score would have overflowed.");
               return;
            end if;
         end;
      end if;
   end loop;

   Put_Line ("Part 2 similarity score: " & Sim_Score'Image);

end Main;

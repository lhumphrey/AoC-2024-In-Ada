with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Main is

   Max_Index : Positive := 50;

   subtype Index is Natural range 1 .. Max_Index;
   subtype Extended_Index is Natural range 0 .. Max_Index + 1;

   function Character_Hash (C : Character) return Ada.Containers.Hash_Type is
      (Character'Pos (C));

   package Character_Sets is new
     Ada.Containers.Hashed_Sets (Character, Character_Hash, "=", "=");

   type Antinode_Array is array
     (Index'Range, Index'Range) of Character_Sets.Set;

   procedure Update_Antinodes_With_No_Harmonics
     (I, II, J, JJ : in Index;
      C : in Character;
      Arr : in out Antinode_Array)
   is
      dy : Integer := II - I;
      dx : Integer := JJ - J;
   begin
      if I - dy in Arr'Range (1) and then J - dx in Arr'Range (2) then
         Arr (I - dy, J - dx).Include (C);
      end if;
      if II + dy in Arr'Range (1) and then JJ + dx in Arr'Range (2) then
         Arr (II + dy, JJ + dx).Include (C);
      end if;
   end Update_Antinodes_With_No_Harmonics;

   procedure Update_Antinodes_With_Harmonics
     (I, II, J, JJ : in Index;
      C : in Character;
      Arr : in out Antinode_Array)
   is
      dy : Integer := II - I;
      dx : Integer := JJ - J;
      mx, my : Integer := 0;
   begin
      while I - my in Arr'Range (1) and then J - mx in Arr'Range (2) loop
         Arr (I - my, J - mx).Include (C);
         my := my + dy;
         mx := mx + dx;
      end loop;
      mx := 0;
      my := 0;
      while II + my in Arr'Range (1) and then JJ + mx in Arr'Range (2) loop
         Arr (II + my, JJ + mx).Include (C);
         mx := mx + dx;
         my := my + dy;
      end loop;
   end Update_Antinodes_With_Harmonics;

   Filename : String := "input.txt";
   File : File_Type;
   Line_Num : Positive := 1;
   Antenna_Array : array (Index'Range, Index'Range) of Character;
   Antinode_Array_1, Antinode_Array_2 : Antinode_Array;
   Total_1, Total_2 : Natural := 0;

begin
   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      declare
         S : String (1 .. Extended_Index'Last);
         Idx, S_Last : Natural := 1;
         C : Character;
      begin
         Get_Line (File, S, S_Last);
         while Idx <= S_Last loop
            C := S (Idx);
            Antenna_Array (Line_Num, Idx) := C;
            Idx := Idx + 1;
         end loop;
      end;
      Line_Num := Line_Num + 1;
   end loop;

   for I in Antenna_Array'Range (1) loop
      for J in Antenna_Array'Range (1) loop
         declare
            C1, C2 : Character;
         begin
            C1 := Antenna_Array (I, J);
            if C1 /= '.' then
               for JJ in J + 1 .. Index'Last loop
                  C2 := Antenna_Array (I, JJ);
                  if C1 = C2 then
                     Update_Antinodes_With_No_Harmonics
                       (I, I, J, JJ, C1, Antinode_Array_1);
                     Update_Antinodes_With_Harmonics
                       (I, I, J, JJ, C1, Antinode_Array_2);
                  end if;
               end loop;
               for II in I + 1 .. Antenna_Array'Last (1) loop
                  for JJ in Antenna_Array'Range (2) loop
                     C2 := Antenna_Array (II, JJ);
                     if C1 = C2 then
                        Update_Antinodes_With_No_Harmonics
                          (I, II, J, JJ, C1, Antinode_Array_1);
                        Update_Antinodes_With_Harmonics
                          (I, II, J, JJ, C1, Antinode_Array_2);
                     end if;
                  end loop;
               end loop;
            end if;
         end;
      end loop;
   end loop;

   for I in Antinode_Array'Range (1) loop
      for J in Antinode_Array'Range (2) loop
         if not Antinode_Array_1 (I, J).Is_Empty then
            Total_1 := Total_1 + 1;
         end if;
         if not Antinode_Array_2 (I, J).Is_Empty then
            Total_2 := Total_2 + 1;
         end if;
      end loop;
   end loop;

   Put_Line ("Total unique antinode locations without harmonics for Part 1: "
             & Total_1'Image);
   Put_Line ("Total unique antinode locations with harmonics for Part 2: "
             & Total_2'Image);

   for I in Antenna_Array'Range(1) loop
      for J in Antenna_Array'Range(2) loop
         Put (Antenna_Array (I, J));
      end loop;
      New_Line;
   end loop;
   New_Line;

   for I in Antinode_Array'Range(1) loop
      for J in Antinode_Array'Range(2) loop
         if not Antinode_Array_2 (I, J).Is_Empty then
            Put ('0');
         else
            Put ('.');
         end if;
      end loop;
      New_Line;
   end loop;

end;

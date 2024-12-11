with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Main is

   package Big_Int_Vectors is new
     Ada.Containers.Vectors (Positive, Big_Integer);
   use Big_Int_Vectors;

   Filename : String := "input.txt";
   File : File_Type;
   Total_1, Total_2 : Big_Integer := 0;

   function Has_Match_1
     (Target_Num, Front_Num : Big_Integer;
      Num_List : Big_Int_Vectors.Vector) return Boolean
   is
      Remaining_Num_List : Big_Int_Vectors.Vector := Num_List.Copy;
      Next_Num : Big_Integer;
   begin
      if Remaining_Num_List.Is_Empty then
         return Target_Num = Front_Num;
      else
         Next_Num := First_Element (Remaining_Num_List);
         Delete_First (Remaining_Num_List);
         return
           Has_Match_1 (Target_Num, Front_Num * Next_Num, Remaining_Num_List)
           or else
           Has_Match_1 (Target_Num, Front_Num + Next_Num, Remaining_Num_List);
      end if;
   end Has_Match_1;

   function Has_Match_2
     (Target_Num, Front_Num : Big_Integer;
      Num_List : Big_Int_Vectors.Vector) return Boolean
   is
      Remaining_Num_List : Big_Int_Vectors.Vector := Num_List.Copy;
      Next_Num : Big_Integer;
   begin
      if Remaining_Num_List.Is_Empty then
         return Target_Num = Front_Num;
      else
         Next_Num := First_Element (Remaining_Num_List);
         Delete_First (Remaining_Num_List);
         declare
            Front_Num_Str : String := Trim (To_String (Front_Num), Both);
            Next_Num_Str : String := Trim (To_String (Next_Num), Both);
         begin
            return Has_Match_2 (Target_Num, Front_Num * Next_Num, Remaining_Num_List) or else
              Has_Match_2 (Target_Num, Front_Num + Next_Num, Remaining_Num_List) or else
              Has_Match_2 (Target_Num, From_String (Front_Num_Str & Next_Num_Str), Remaining_Num_List);
         end;
      end if;
   end Has_Match_2;

begin
   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      declare
         S : String (1 .. 256) := (others => ' ');
         Idx : Natural := 1;
         S_Last : Natural;
         Digit_RE : Pattern_Matcher := Compile("(\d+)");
         M : Match_Array (0 .. 1);
         Num_List : Big_Int_Vectors.Vector;
         Target_Num, Num, Front_Num : Big_Integer;
      begin
         Get_Line (File, S, S_Last);
         Match (Digit_RE, S, M, Idx);
         Target_Num := From_String (S (M (1).First .. M (1).Last));
         Idx := M(0).Last + 1;
         while Idx <= S'Last loop
            Match (Digit_RE, S, M, Idx);
            if M(0) /= No_Match then
               Num := From_String (S (M (1).First .. M (1).Last));
               Num_List.Append (Num);
               Idx := M (0).Last + 1;
            else
               Idx := S'Last + 1;
            end if;
         end loop;
         Front_Num := First_Element (Num_List);
         Delete_First (Num_List);
         if Has_Match_1 (Target_Num, Front_Num, Num_List) then
            Total_1 := Total_1 + Target_Num;
            Total_2 := Total_2 + Target_Num;
         elsif Has_Match_2 (Target_Num, Front_Num, Num_List) then
            Total_2 := Total_2 + Target_Num;
         end if;
      end;
   end loop;

   Put_Line ("Total for Part 1: " & Total_1'Image);
   Put_Line ("Total for Part 2: " & Total_2'Image);

end;

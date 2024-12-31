with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with GNAT.Regpat; use GNAT.Regpat;

procedure Main is

   package Natural_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Positive);
   use Natural_Vectors;

   type Natural_Vector_Acc is access Natural_Vectors.Vector;

   function My_Hash (P : Positive) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (P mod 100));

   package Vector_Acc_Hashed_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Positive,
                                 Element_Type    => Natural_Vector_Acc,
                                 Hash            => My_Hash,
                                 Equivalent_Keys => "=");
   use Vector_Acc_Hashed_Maps;

   Re_Pair : constant Pattern_Matcher := Compile ("(\d+)\|(\d+)");
   Re_Digit : constant Pattern_Matcher := Compile ("(\d+),*");
   M : Match_Array (0 .. 2);

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 128);
   S_Last : Natural := 0;
   Page_Map : Vector_Acc_Hashed_Maps.Map;
   N1, N2 : Positive;
   Total1, Total2 : Natural := 0;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, S_Last);
      Match (Re_Pair, S (1 .. S_Last), M);
      if M (0) /= No_Match then
         N1 := Positive'Value (S (M (1).First .. M (1).Last));
         N2 := Positive'Value (S (M (2).First .. M (2).Last));
         if not Page_Map.Contains (N1) then
            declare
               V : Natural_Vector_Acc := new Natural_Vectors.Vector;
            begin
               V.Append (N2);
               Page_Map.Insert (N1, V);
            end;
         else
            Page_Map.Element (N1).Append (N2);
         end if;
      else
         exit;
      end if;
   end loop;

   while not End_Of_File (File) loop
      Get_Line (File, S, S_Last);
      declare
         V : Natural_Vectors.Vector;
         E1, E2 : Positive;
         Idx : Positive := 1;
         Half_Idx : Positive;
      begin
         Match (Re_Digit, S (Idx .. S_Last), M);
         while M (0) /= No_Match and then Idx < S_Last loop
            V.Append (Positive'Value
                      (S (M (1).First .. M (1).Last)));
            Idx := M (1).Last + 1;
            Match (Re_Digit, S (Idx .. S_Last), M);
         end loop;

         if (for all I in 1 .. V.Last_Index =>
               (for all J in 1 .. I - 1 =>
                  Page_Map.Element (V.Element (I)).Find_Index (V.Element (J)) =
                    0))
         then
            Half_Idx := (1 + V.Last_Index) / 2;
            Total1 := Total1 + V.Element (Half_Idx);
         else
            for I in 1 .. V.Last_Index loop
               E1 := V.Element (I);
               for J in I + 1 .. V.Last_Index loop
                  E2 := V.Element (J);
                  if Page_Map.Element (E2).Find_Index (E1) /= 0 then
                     for K in 1 .. I loop
                        if Page_Map.Element (V.Element (K)).Find_Index (E2) = 0 then
                           V.Delete (J);
                           V.Insert (K, E2);
                           exit;
                        end if;
                     end loop;
                  end if;
               end loop;
            end loop;
            Half_Idx := (1 + V.Last_Index) / 2;
            Total2 := Total2 + V.Element (Half_Idx);
         end if;
      end;
   end loop;

   Put_Line ("Total for correct lines: " & Total1'Image);
   Put_Line ("Total for incorrect then sorted lines: " & Total2'Image);

   Close (File);

end;

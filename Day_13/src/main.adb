with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Containers.Vectors;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Main is

   subtype Two_By_Two_Matrix is Real_Matrix (1 .. 2, 1 .. 2);

   subtype Two_Vector is Real_Vector (1 .. 2);

   type Claw_Machine is record
      M : Two_By_Two_Matrix;
      Prize_Loc : Two_Vector;
   end record;

   package Claw_Machine_Vectors is new
     Ada.Containers.Vectors (Positive, Claw_Machine);

   Filename : constant String := "input.txt";
   File : File_Type;
   Claw_Machine_List : Claw_Machine_Vectors.Vector;
   A_Cost : constant Integer := 3;
   B_Cost : constant Integer := 1;
   Total_1 : Integer := 0;
   Total_2 : Big_Integer := 0;
begin

   Open (File, In_File, Filename);

   declare
      S : String (1 .. 256);
      S_Last : Natural;
      Button_Values : Pattern_Matcher := GNAT.Regpat.Compile ("Button [AB]: X\+(\d+), Y\+(\d+)");
      Prize_Location : Pattern_Matcher := GNAT.Regpat.Compile ("Prize: X=(\d+), Y=(\d+)");
      Matches : Match_Array (0 .. 2);
      M : Two_By_Two_Matrix;
      CM : Claw_Machine;
   begin
      while not End_Of_File (File) loop
         Get_Line (File, S, S_Last);
         Match (Button_Values, S (1 .. S_Last), Matches);
         M (1, 1) := Float'Value (S (Matches (1).First .. Matches (1).Last));
         M (2, 1) := Float'Value (S (Matches (2).First .. Matches (2).Last));
         Get_Line (File, S, S_Last);
         Match (Button_Values, S (1 .. S_Last), Matches);
         M (1, 2) := Float'Value (S (Matches (1).First .. Matches (1).Last));
         M (2, 2) := Float'Value (S (Matches (2).First .. Matches (2).Last));
         CM.M := M;
         Get_Line (File, S, S_Last);
         Match (Prize_Location, S (1 .. S_Last), Matches);
         CM.Prize_Loc (1) := Float'Value (S (Matches (1).First .. Matches (1).Last));
         CM.Prize_Loc (2) := Float'Value (S (Matches (2).First .. Matches (2).Last));
         Claw_Machine_List.Append (CM);
         if not End_Of_File (File) then
            Get_Line (File, S, S_Last);
         end if;
      end loop;
   end;

   Close (File);

   declare
      Result : Two_Vector;
      N_A, N_B : Float;
      N_A_Big, N_B_Big : Big_Integer;
      function eps (X, Y : Float) return Boolean is (abs (X - Y) < 0.001);
      function det (M : Two_By_Two_Matrix) return Big_Integer is
        (To_Big_Integer ((Integer (M (1, 1)) * Integer (M (2, 2)) -
                            Integer (M (2, 1)) * Integer (M (1, 2)))));
      function TBI (F : Float) return Big_Integer is (To_Big_Integer (Integer (F)));
   begin
      for E of Claw_Machine_List loop
         Result := Inverse (E.M) * E.Prize_Loc;
         N_A := Float'Rounding (Result (1));
         N_B := Float'Rounding (Result (2));
         if (eps (N_A, Result (1)) and eps (N_B, Result (2))) then
            Total_1 := Total_1 + A_Cost * Integer (N_A) + B_Cost * Integer (N_B);
         end if;

         N_A_Big := TBI (E.M (2, 2)) * (TBI (E.Prize_Loc (1)) + 10000000000000) +
           TBI (-E.M (1, 2)) * (TBI (E.Prize_Loc (2)) + 10000000000000);
         N_B_Big := TBI (-E.M (2, 1)) * (TBI (E.Prize_Loc (1)) + 10000000000000) +
           TBI (E.M (1, 1)) * (TBI (E.Prize_Loc (2)) + 10000000000000);
         if (N_A_Big mod det (E.M) = 0 and then N_B_Big mod det (E.M) = 0)
         then
            Total_2 := Total_2 + To_Big_Integer (A_Cost) * (N_A_Big / det (E.M)) +
              To_Big_Integer (B_Cost) * (N_B_Big / det (E.M));
         end if;
      end loop;
   end;

   Put_Line ("Total number of tokens for Part 1: " & Total_1'Image);
   Put_Line ("Total number of tokens for Part 2: " & Total_2'Image);

end Main;

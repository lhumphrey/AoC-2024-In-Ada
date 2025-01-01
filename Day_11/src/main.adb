with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is

   package Big_Integer_To_Integer is new Signed_Conversions (Integer);

   function Big_Integer_Hash (BI : Big_Integer) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type
       (Big_Integer_To_Integer.From_Big_Integer (BI mod To_Big_Integer (2**31 - 1))));

   package Big_Integer_To_Big_Integer_Hash_Maps is new
     Ada.Containers.Hashed_Maps (Big_Integer, Big_Integer, Big_Integer_Hash, "=");
   use Big_Integer_To_Big_Integer_Hash_Maps;

   package Big_Integer_Vectors is new Ada.Containers.Vectors (Positive, Big_Integer);
   use Big_Integer_Vectors;


   function Initialize_Map (S : String) return Big_Integer_To_Big_Integer_Hash_Maps.Map is
      M : Big_Integer_To_Big_Integer_Hash_Maps.Map;
      Num : Integer;
      Last : Integer := S'First;
   begin
      while Last in S'Range loop
         Ada.Integer_Text_IO.Get (S (Last .. S'Last), Num, Last);
         if not M.Contains (To_Big_Integer (Num)) then
            M.Insert (To_Big_Integer (Num), 1);
         else
            M.Replace (To_Big_Integer (Num), M.Element (To_Big_Integer (Num)) + 1);
         end if;
         Last := Last + 1;
      end loop;
      return M;
   end Initialize_Map;


   function Number_Of_Digits (B : Big_Integer) return Natural is
      S : String := Ada.Strings.Fixed.Trim (To_String (B), Ada.Strings.Both);
   begin
      return S'Length;
   end Number_Of_Digits;


   procedure Add_Stones_To_Map
     (M : in out Big_Integer_To_Big_Integer_Hash_Maps.Map;
      Stone_Value : Big_Integer;
      Number_Of_Stones : Big_Integer) is
   begin
      if M.Contains (Stone_Value) then
         M (Stone_Value) := M (Stone_Value) + Number_Of_Stones;
      else
         M.Insert (Stone_Value, Number_Of_Stones);
      end if;
   end Add_Stones_To_Map;


   procedure Update_Map (M : in out Big_Integer_To_Big_Integer_Hash_Maps.Map) is
      Stone_Values : Big_Integer_Vectors.Vector;
      Stone_Counts : Big_Integer_Vectors.Vector;
   begin
      for C in M.Iterate loop
         Stone_Values.Append (Key (C));
         Stone_Counts.Append (Element (C));
      end loop;

      for I in Stone_Values.First_Index .. Stone_Values.Last_Index loop
         M (Stone_Values (I)) := M (Stone_Values (I)) - Stone_Counts (I);
         if Stone_Values (I) = 0 then
            Add_Stones_To_Map (M, 1, Stone_Counts (I));
         elsif Number_Of_Digits (Stone_Values (I)) mod 2 = 0 then
            declare
               S : String := Ada.Strings.Fixed.Trim (To_String (Stone_Values (I)), Ada.Strings.Both);
            begin
               Add_Stones_To_Map (M, From_String (S (1 .. S'Last / 2)), Stone_Counts (I));
               Add_Stones_To_Map (M, From_String (S (S'Last / 2 + 1 .. S'Last)), Stone_Counts (I));
            end;
         else
            Add_Stones_To_Map (M, Stone_Values (I) * 2024, Stone_Counts (I));
         end if;
      end loop;

      for Stone_Value of Stone_Values loop
         if M (Stone_Value) = 0 then
            M.Delete (Stone_Value);
         end if;
      end loop;
   end Update_Map;


   procedure Print_Map (M : in Big_Integer_To_Big_Integer_Hash_Maps.Map) is
   begin
      for C in M.Iterate loop
         Put (To_String (Key (C)) & " (" & To_String (M.Element (Key (C))) & ") ");
      end loop;
      New_Line;
   end Print_Map;


   Filename : String := "input.txt";
   File : File_Type;
   M : Big_Integer_To_Big_Integer_Hash_Maps.Map;
   Number_Of_Stones : Big_Integer := 0;

begin

   Open (File, In_File, Filename);

   declare
      S : String := Get_Line (File);
   begin
      M := Initialize_Map (S);

      for I in 1 .. 25 loop
         Update_Map (M);
      end loop;

      for E of M loop
         Number_Of_Stones := Number_Of_Stones + E;
      end loop;

      Put_Line ("Number of stones after 25 blinks: " & To_String (Number_Of_Stones));

      for I in 1 .. 50 loop
         Update_Map (M);
      end loop;

      Number_Of_Stones := 0;
      for E of M loop
         Number_Of_Stones := Number_Of_Stones + E;
      end loop;

      Put_Line ("Number of stones after 75 blinks: " & To_String (Number_Of_Stones));
   end;

   Close (File);

end Main;

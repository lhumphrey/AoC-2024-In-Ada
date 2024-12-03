with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main with SPARK_Mode is

   package Natural_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Natural);
   use Natural_Vectors;

   type Natural_Vector_Acc is access Natural_Vectors.Vector;

   package Natural_Vector_Acc_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Positive, Element_Type => Natural_Vector_Acc);
   use Natural_Vector_Acc_Vectors;

   function Is_Increasing (V : Natural_Vectors.Vector) return Boolean is
     (for all I in First_Index (V) + 1 .. Last_Index (V) =>
           V.Element (I) > V.Element (I - 1));

   function Is_Decreasing (V : Natural_Vectors.Vector) return Boolean is
     (for all I in First_Index (V) + 1 .. Last_Index (V) =>
           V.Element (I) < V.Element (I - 1));

   function Is_Gradual (V : Natural_Vectors.Vector) return Boolean is
     (for all I in First_Index (V) + 1 .. Last_Index (V) =>
           abs (V.Element (I) - V.Element (I - 1)) <= 3);

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 256);
   Last_Idx : Natural;
   Num_Safe_Reports : Natural := 0;
   Vec : Natural_Vector_Acc_Vectors.Vector;
begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, Last_Idx);

      if Last_Idx > 0 then
         declare
            Sub_Vec : Natural_Vector_Acc := new Natural_Vectors.Vector;
            Idx : Natural := 1;
            Num : Natural;
         begin
            while Idx <= Last_Idx loop
               Get (S (Idx .. Last_Idx), Num, Idx);
               Sub_Vec.Append (Num);
               Idx := Idx + 1;
            end loop;
            Vec.Append(Sub_Vec);
         end;
      end if;

   end loop;

   Close (File);

   for E of Vec loop
      if (Is_Increasing (E.all) or Is_Decreasing (E.all))
        and Is_Gradual (E.all)
      then
         Num_Safe_Reports := Num_Safe_Reports + 1;
      end if;
   end loop;

   Put_Line("Part 1 number of safe reports: " & Num_Safe_Reports'Image);

end;

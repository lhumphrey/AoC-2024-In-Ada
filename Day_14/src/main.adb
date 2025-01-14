with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Containers.Vectors;

procedure Main is

   subtype X_Coord is Integer range 0 .. 100;

   subtype Y_Coord is Integer range 0 .. 102;

   type Robot is record
      X : X_Coord;
      Y : Y_Coord;
      V_X, V_Y : Integer := 0;
   end record;

   type Natural_Array_1x4 is array (1 .. 4) of Natural;

   type Natural_Array is array (X_Coord, Y_Coord) of Natural;

   subtype Percentage is Natural range 0 .. 100;

   procedure Move_Robot (R : in out Robot) is
   begin
      R.X := (R.X + R.V_X) mod (X_Coord'Last + 1);
      R.Y := (R.Y + R.V_Y) mod (Y_Coord'Last + 1);
   end Move_Robot;

   function Quad_Number (R : Robot) return Natural is
   begin
      if R.X < X_Coord'Last / 2 and R.Y < Y_Coord'Last / 2 then
         return 1;
      elsif R.X > X_Coord'Last / 2 and R.Y < Y_Coord'Last / 2 then
         return 2;
      elsif R.X < X_Coord'Last / 2 and R.Y > Y_Coord'Last / 2 then
         return 3;
      elsif R.X > X_Coord'Last / 2 and R.Y > Y_Coord'Last / 2 then
         return 4;
      else
         return 0;
      end if;
   end Quad_Number;

   package Robot_Vectors is new Ada.Containers.Vectors (Positive, Robot);
   use Robot_Vectors;

   function Tally_Quads
     (Robots : Robot_Vectors.Vector) return Natural_Array_1x4
   is
      Result : Natural_Array_1x4 := (others => 0);
      N : Natural;
   begin
      for E of Robots loop
         N := Quad_Number (E);
         if N > 0 then
            Result (N) := Result (N) + 1;
         end if;
      end loop;
      return Result;
   end Tally_Quads;

   function Max_Overlapping_Robots
     (Robots_Per_Square : Natural_Array) return Positive
   is
      Max : Positive := 1;
   begin
      for I in Robots_Per_Square'Range (1) loop
         for J in Robots_Per_Square'Range (2) loop
            if Robots_Per_Square (I, J) > Max then
               Max := Robots_Per_Square (I, J);
            end if;
         end loop;
      end loop;
      return Max;
   end Max_Overlapping_Robots;

   function Get_Robots_Per_Square
     (Robots : Robot_Vectors.Vector) return Natural_Array
   is
      Result : Natural_Array := (others => (others => 0));
   begin
      for E of Robots loop
         Result (E.X, E.Y) := Result (E.X, E.Y) + 1;
      end loop;
      return Result;
   end Get_Robots_Per_Square;

   procedure Plot_Robot_Map (Robots_Per_Square : Natural_Array) is
   begin
      for Y in Robots_Per_Square'Range(2) loop
         for X in Robots_Per_Square'Range(1) loop
            Put ((if Robots_Per_Square (X, Y) > 0 then '*' else '.'));
         end loop;
         New_Line;
      end loop;
   end Plot_Robot_Map;

   function Single_Quad_Exceeds_Percent
     (Quad_Tally : Natural_Array_1x4;
      Num_Robots : Positive;
      Percent : Percentage) return Boolean is
   begin
      return (100 * Quad_Tally (1) / Num_robots >= Percent or else
              100 * Quad_Tally (2) / Num_robots >= Percent or else
              100 * Quad_Tally (3) / Num_robots >= Percent or else
              100 * Quad_Tally (4) / Num_robots >= Percent);
   end;

   Filename : constant String := "input.txt";
   File : File_Type;
   Robots : Robot_Vectors.Vector;
   Total_1 : Integer := 0;
   Time_Part_2 : Positive := Positive'Last;
   Quad_Tally : Natural_Array_1x4;
begin

   Open (File, In_File, Filename);

   declare
      S : String (1 .. 256);
      S_Last : Natural;
      Position_Values : Pattern_Matcher := GNAT.Regpat.Compile
        ("p=(\d+),(\d+) v=(\-{0,1}\d+),(\-{0,1}\d+)");
      Matches : Match_Array (0 .. 4);
      R : Robot;
   begin
      while not End_Of_File (File) loop
         Get_Line (File, S, S_Last);
         Match (Position_Values, S (1 .. S_Last), Matches);
         R.X := X_Coord'Value (S (Matches (1).First .. Matches (1).Last));
         R.Y := Y_Coord'Value (S (Matches (2).First .. Matches (2).Last));
         R.V_X := Integer'Value (S (Matches (3).First .. Matches (3).Last));
         R.V_Y := Integer'Value (S (Matches (4).First .. Matches (4).Last));
         Robots.Append (R);
      end loop;
   end;

   Close (File);

   for Time in 1 .. 100 loop
      for I in Robots.First_Index .. Robots.Last_Index loop
         Robots.Update_Element (I, Move_Robot'Access);
      end loop;
      Quad_Tally := Tally_Quads (Robots);
      if Single_Quad_Exceeds_Percent (Quad_Tally, Integer (Length (Robots)), 45) then
         if Time < Time_Part_2 then
            Time_Part_2 := Time;
         end if;
         Put_Line ("Time: " & Time'Image);
         Plot_Robot_Map (Get_Robots_Per_Square (Robots));
         New_Line;
      end if;
   end loop;

   Quad_Tally := Tally_Quads (Robots);
   Total_1 := Quad_Tally (1) * Quad_Tally (2) * Quad_Tally (3) * Quad_Tally (4);

   for Time in 101 .. 10000 loop
      for I in Robots.First_Index .. Robots.Last_Index loop
         Robots.Update_Element (I, Move_Robot'Access);
      end loop;
      Quad_Tally := Tally_Quads (Robots);
      if Single_Quad_Exceeds_Percent (Quad_Tally, Integer (Length (Robots)), 45) then
         if Time < Time_Part_2 then
            Time_Part_2 := Time;
         end if;
         Put_Line ("Time: " & Time'Image);
         Plot_Robot_Map (Get_Robots_Per_Square (Robots));
         New_Line;
      end if;
   end loop;

   Put_Line ("Safety factor for Part 1: " & Total_1'Image);
   Put_Line ("Easter egg time for Part 2: " & Time_Part_2'Image);

end Main;

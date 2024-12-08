with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with GNAT.Regpat; use GNAT.Regpat;

procedure Main is

   subtype Coord is Integer range 1 .. 130;

   subtype Extended_Coord is Integer range 0 .. 131;

   type Direction is (Up, Right, Down, Left);

   function Change_Direction (D: Direction) return Direction is
     (case D is when Up => Right, when Right => Down,
         when Down => Left, when Left => Up);

   package Coordinate_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Coord);
   use Coordinate_Vectors;

   type Coordinate_Vector_Acc is access Coordinate_Vectors.Vector;

   type Obstacle_Coord_Lists is
     array (Positive range <>) of Coordinate_Vector_Acc;

   type Boolean_Array is array (Coord'Range) of Boolean;

   type Boolean_Array_2D is array (Coord'Range) of Boolean_Array;

   procedure Move_To_Closest_Obstacle
     (Dir : in out Direction;
      Obstacle_Coords_In_Row,
      Obstacle_Coords_In_Col : Coordinate_Vectors.Vector;
      X_Coord, Y_Coord : in out Extended_Coord;
      Visited : in out Boolean_Array_2D)
   is
      New_Coord : Extended_Coord;
   begin
      case Dir is
         when Down =>
            New_Coord := Extended_Coord'Last;
            for Obstacle_Coord of Obstacle_Coords_In_Col loop
               if Obstacle_Coord > Y_Coord then
                  New_Coord := Obstacle_Coord - 1;
                  exit;
               end if;
            end loop;
            for Y in Y_Coord .. Coord'Min (Coord'Last, New_Coord) loop
               Visited (X_Coord) (Y) := True;
            end loop;
            Y_Coord := New_Coord;
            Dir := Change_Direction (Dir);
         when Right =>
            New_Coord := Extended_Coord'Last;
            for Obstacle_Coord of Obstacle_Coords_In_Row loop
               if Obstacle_Coord > X_Coord then
                  New_Coord := Obstacle_Coord - 1;
                  exit;
               end if;
            end loop;
            for X in X_Coord .. Coord'Min (Coord'Last, New_Coord) loop
               Visited (X) (Y_Coord) := True;
            end loop;
            X_Coord := New_Coord;
            Dir := Change_Direction (Dir);
         when Up =>
            New_Coord := 0;
            for Obstacle_Coord of Obstacle_Coords_In_Col loop
               if Obstacle_Coord < Y_Coord then
                  New_Coord := Obstacle_Coord + 1;
               end if;
            end loop;
            for Y in Coord'Max (Coord'First, New_Coord) .. Y_Coord loop
               Visited (X_Coord) (Y) := True;
            end loop;
            Y_Coord := New_Coord;
            Dir := Change_Direction (Dir);
         when Left =>
            New_Coord := 0;
            for Obstacle_Coord of Obstacle_Coords_In_Row loop
               if Obstacle_Coord < X_Coord then
                  New_Coord := Obstacle_Coord + 1;
               end if;
            end loop;
            for X in Coord'Max (Coord'First, New_Coord) .. X_Coord  loop
               Visited (X) (Y_Coord) := True;
            end loop;
            X_Coord := New_Coord;
            Dir := Change_Direction (Dir);
      end case;
   end Move_To_Closest_Obstacle;

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 131);
   S_Last : Positive;
   Row, Col : Positive := 1;
   Guard_Direction : Direction := Up;
   Guard_X, Guard_Y : Extended_Coord := 0;
   Obstacle_Coords_In_Row,
   Obstacle_Coords_In_Col : Obstacle_Coord_Lists (Coord'Range);
   Visited : Boolean_Array_2D := (others => (others => False));
   Total1 : Natural := 0;
   Draw : array (Coord'Range, Coord'Range) of Character := (others => (others => '.'));

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, S_Last);
      Col := 1;
      while Col <= S_Last loop
         if S (Col) = '#' then
            if Obstacle_Coords_In_Col (Col) = null then
               Obstacle_Coords_In_Col (Col) := new Coordinate_Vectors.Vector;
            end if;
            if Obstacle_Coords_In_Row (Row) = null then
               Obstacle_Coords_In_Row (Row) := new Coordinate_Vectors.Vector;
            end if;
            Obstacle_Coords_In_Col (Col).Append (Row);
            Obstacle_Coords_In_Row (Row).Append (Col);
            Draw (Row, Col) := '#';
         elsif S (Col) = '^' then
            Guard_X := Col;
            Guard_Y := Row;
            Draw (Row, Col) := '^';
         end if;
         Col := Col + 1;
      end loop;
      Row := Row + 1;
   end loop;

   Close (File);

   while Guard_X in Coord'Range and Guard_Y in Coord'Range loop
      Move_To_Closest_Obstacle (Guard_Direction,
                                Obstacle_Coords_In_Row (Guard_Y).all,
                                Obstacle_Coords_In_Col (Guard_X).all,
                                Guard_X,
                                Guard_Y,
                                Visited);
   end loop;

   for I in Visited'Range loop
      for J in Visited (I)'Range loop
         if Visited (I) (J) then
            Total1 := Total1 + 1;
         end if;
      end loop;
   end loop;

   Put_Line ("Unique Positions Visited: " & Total1'Image);

end;

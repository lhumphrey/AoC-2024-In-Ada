with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with GNAT.Regpat; use GNAT.Regpat;

procedure Main is

   subtype Coord is Integer range 1 .. 130;

   subtype Extended_Coord is Integer range 0 .. 131;

   type Direction is (Up, Right, Down, Left);

   package Direction_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Direction);
   use Direction_Sets;

   function Change_Direction (D: Direction) return Direction is
     (case D is when Up => Right, when Right => Down,
         when Down => Left, when Left => Up);

   type Coord_Direction_Set_Array is array
     (Coord'Range) of Direction_Sets.Set;

   type Coord_2D_Direction_Set_Array is array
     (Coord'Range) of Coord_Direction_Set_Array;

   type Coord_Obstacle_Array is array
     (Coord'Range) of Boolean;

   type Coord_2D_Obstacle_Array is array
     (Coord'Range) of Coord_Obstacle_Array;

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. Extended_Coord'Last);
   S_Last : Positive;
   Row, Col : Positive := 1;
   Guard_Direction : Direction := Up;
   Guard_Row_Start, Guard_Row, Guard_Col_Start, Guard_Col : Extended_Coord := 0;
   Has_Obstacle : Coord_2D_Obstacle_Array := (others => (others => False));
   Placed_Obstacle : Coord_2D_Obstacle_Array := (others => (others => False));
   Directions_At_Coord : Coord_2D_Direction_Set_Array;
   Total1 : Natural := 0;
   Obstacle_Count : Natural := 0;
   Draw : array (Coord'Range, Coord'Range) of Character := (others => (others => '.'));

   procedure Move
     (Row, Col : in out Extended_Coord;
      Dir : in out Direction;
      Directions : in out Coord_2D_Direction_Set_Array) is
   begin
      case Dir is
         when Up =>
            Directions (Row) (Col).Include (Up);
            if Row - 1 in Coord'Range and then Has_Obstacle (Row - 1) (Col) then
               Dir := Right;
            else
               Row := Row - 1;
            end if;
         when Right =>
            Directions (Row) (Col).Include (Right);
            if Col + 1 in Coord'Range and then Has_Obstacle (Row) (Col + 1) then
               Dir := Down;
            else
               Col := Col + 1;
            end if;
         when Left =>
            Directions (Row) (Col).Include (Left);
            if Col - 1 in Coord'Range and then Has_Obstacle (Row) (Col - 1) then
              Dir := Up;
            else
              Col := Col - 1;
            end if;
         when Down =>
            Directions (Row) (Col).Include (Down);
            if Row + 1 in Coord'Range and then Has_Obstacle (Row + 1) (Col) then
               Dir := Left;
            else
               Row := Row + 1;
            end if;
      end case;
   end Move;

   function Has_Cycle
     (Row, Col : in out Extended_Coord;
      Dir : in out Direction;
      Directions : in Coord_2D_Direction_Set_Array) return Boolean
   is
      Local_Directions := Directions;
   begin
      while Guard_Row in Coord'Range and Guard_Col in Coord'Range loop
         Move (Row, Col, Dir, Local_Directions);
         if not Empty (Directions (Row) (Col))
           and then Directions (Row) (Col).Contains (Dir)
         then
            return True;
         end if;
      end if;
      end loop;
      return False;
   end loop;
   end Move;

   function Can_Reach_Loop
     (Row, Col : in Coord;
      Dir : in Direction) return Boolean is
   begin
      if (Dir = Up and then Row = Coord'First)
        or else (Dir = Down and then Row = Coord'Last)
        or else (Dir = Left and then Col = Coord'First)
        or else (Dir = Right and then Col = Coord'Last)
      then
         return False;
      end if;

      case Dir is
         when Up =>
            for C in Col + 1 .. Coord'Last - 1 loop
               if Has_Obstacle (Row) (C) then
                  return Can_Reach_Loop (Row, C - 1, Right);
               elsif Directions_At_Coord (Row) (C).Contains (Right) then
                  Placed_Obstacle (Row - 1) (Col) := True;
                  return True;
               end if;
            end loop;
         when Down =>
            for C in reverse Coord'First + 1 .. Col - 1 loop
               if Has_Obstacle (Row) (C) then
                  return Can_Reach_Loop (Row, C + 1, Left);
               elsif Directions_At_Coord (Row) (C).Contains (Left) then
                  Placed_Obstacle (Row + 1) (Col) := True;
                  return True;
               end if;
            end loop;
         when Left =>
            for R in reverse Coord'First + 1 .. Row - 1 loop
               if Has_Obstacle (R) (Col) then
                  return Can_Reach_Loop (R + 1, Col, Up);
               elsif Directions_At_Coord (R) (Col).Contains (Up) then
                  Placed_Obstacle (Row) (Col - 1) := True;
                  return True;
               end if;
            end loop;
         when Right =>
            for R in Row + 1 .. Coord'Last - 1 loop
               if Has_Obstacle (R) (Col) then
                  return Can_Reach_Loop (R - 1, Col, Down);
               elsif Directions_At_Coord (R) (Col).Contains (Down) then
                  Placed_Obstacle (Row) (Col + 1) := True;
                  return True;
               end if;
            end loop;
      end case;

      return False;
   end Can_Reach_Loop;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, S_Last);
      Col := 1;
      while Col <= S_Last loop
         if S (Col) = '#' then
            Has_Obstacle (Row) (Col) := True;
         elsif S (Col) = '^' then
            Guard_Col_Start := Col;
            Guard_Col := Guard_Col_Start;
            Guard_Row_Start := Row;
            Guard_Row := Guard_Row_Start;
            Directions_At_Coord (Guard_Row) (Guard_Col).Include (Up);
         end if;
         Col := Col + 1;
      end loop;
      Row := Row + 1;
   end loop;

   Close (File);

   while Guard_Row in Coord'Range and Guard_Col in Coord'Range loop
      Move (Guard_Row, Guard_Col, Guard_Direction);
   end loop;

   Guard_Row := Guard_Row_Start;
   Guard_Col := Guard_Col_Start;
   Guard_Direction := Up;

   --  while Guard_Row in Coord'Range and Guard_Col in Coord'Range loop
   --     case Guard_Direction is
   --        when Up =>
   --           Directions_At_Coord (Guard_Row) (Guard_Col).Include (Up);
   --           if Guard_Row - 1 in Coord'Range and then
   --             Has_Obstacle (Guard_Row - 1) (Guard_Col)
   --           then
   --             Guard_Direction := Right;
   --           else
   --              if Can_Reach_Loop (Guard_Row, Guard_Col, Guard_Direction) then
   --                 Obstacle_Count := Obstacle_Count + 1;
   --              end if;
   --              Guard_Row := Guard_Row - 1;
   --           end if;
   --        when Right =>
   --           Directions_At_Coord (Guard_Row) (Guard_Col).Include (Right);
   --           if Guard_Col + 1 in Coord'Range and then
   --             Has_Obstacle (Guard_Row) (Guard_Col + 1)
   --           then
   --             Guard_Direction := Down;
   --           else
   --              if Can_Reach_Loop (Guard_Row, Guard_Col, Guard_Direction) then
   --                 Obstacle_Count := Obstacle_Count + 1;
   --              end if;
   --
   --              Guard_Col := Guard_Col + 1;
   --           end if;
   --        when Left =>
   --           Directions_At_Coord (Guard_Row) (Guard_Col).Include (Left);
   --           if Guard_Col - 1 in Coord'Range and then
   --             Has_Obstacle (Guard_Row) (Guard_Col - 1)
   --           then
   --             Guard_Direction := Up;
   --           else
   --              if Can_Reach_Loop (Guard_Row, Guard_Col, Guard_Direction) then
   --                 Obstacle_Count := Obstacle_Count + 1;
   --              end if;
   --              Guard_Col := Guard_Col - 1;
   --           end if;
   --        when Down =>
   --           Directions_At_Coord (Guard_Row) (Guard_Col).Include (Down);
   --           if Guard_Row + 1 in Coord'Range and then
   --             Has_Obstacle (Guard_Row + 1) (Guard_Col)
   --           then
   --             Guard_Direction := Left;
   --           else
   --              if Can_Reach_Loop (Guard_Row, Guard_Col, Guard_Direction) then
   --                 Obstacle_Count := Obstacle_Count + 1;
   --              end if;
   --              Guard_Row := Guard_Row + 1;
   --           end if;
   --     end case;
   --     Put_Line ("Obstacle_Count : " & Obstacle_Count'Image);
   --  end loop;



   --  for I in Directions_At_Coord'Range loop
   --     for J in Directions_At_Coord (I)'Range loop
   --        if Placed_Obstacle (I) (J) then
   --           Put ("+");
   --        elsif Has_Obstacle (I) (J) then
   --           Put ("#");
   --        elsif Directions_At_Coord (I) (J).Contains (Up) then
   --           Put ("U");
   --        elsif Directions_At_Coord (I) (J).Contains (Down) then
   --           Put ("D");
   --        elsif Directions_At_Coord (I) (J).Contains (Left) then
   --           Put ("L");
   --        elsif Directions_At_Coord (I) (J).Contains (Right) then
   --           Put ("R");
   --        else
   --           Put (".");
   --        end if;
   --     end loop;
   --     New_Line;
   --  end loop;

   for I in Directions_At_Coord'Range loop
      for J in Directions_At_Coord (I)'Range loop
         if Integer (Directions_At_Coord (I) (J).Length) > 0 then
            Total1 := Total1 + 1;
         end if;
      end loop;
   end loop;

   Put_Line ("Unique Positions Visited: " & Total1'Image);

   Put_Line ("Obstacles that can be placed: " & Obstacle_Count'Image);
   -- 437 is too low
   -- Derek's code gives 1711
end;

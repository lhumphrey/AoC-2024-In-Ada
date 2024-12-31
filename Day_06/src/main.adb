with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Hashed_Sets;
with GNAT.Regpat; use GNAT.Regpat;

procedure Main is

   subtype Coordinate is Integer range 1 .. 130;

   subtype Extended_Coordinate is Integer range 0 .. 131;

   type Direction is (Up, Right, Down, Left);

   type Pose is record
      Row, Col : Extended_Coordinate := 0;
      Dir : Direction;
   end record;

   package Pose_Vectors is new Ada.Containers.Vectors(Positive, Pose);
   use Pose_Vectors;

   function Pose_Hash (P : Pose) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type
        (P.Row + P.Col*150 + 20000 * (case P.Dir is
            when Up => 1, when Down => 2, when Left => 3, when Right => 4)));

   package Pose_Sets is new
     Ada.Containers.Bounded_Hashed_Sets (Pose, Pose_Hash, "=", "=");
   use Pose_Sets;

   type Boolean_Array is array (Coordinate'Range, Coordinate'Range) of Boolean;

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. Extended_Coordinate'Last);
   S_Last : Positive;
   Row, Col : Positive := 1;
   Guard_Dir : Direction := Up;
   Guard_Row, Guard_Col : Extended_Coordinate := 0;
   Visited : Boolean_Array := (others => (others => False));
   Has_Obstacle : Boolean_Array := (others => (others => False));
   Placed_Obstacle : Boolean_Array := (others => (others => False));
   Total_Visited, Total_Obstacles_Placed : Natural := 0;
   Counter : Natural := 0;


   procedure Visit_If_Valid
     (Row, Col : in Extended_Coordinate;
      Visited : in out Boolean_Array) is
   begin
      if Row in Coordinate'Range and Col in Coordinate'Range then
         Visited (Row, Col) := True;
      end if;
   end Visit_If_Valid;


   function Change_Direction (D: Direction) return Direction is
     (case D is
         when Up => Right, when Right => Down,
         when Down => Left, when Left => Up);

   procedure Change_Direction (P : in out Pose) is
   begin
      P := Pose'(P.Row, P.Col, Change_Direction (P.Dir));
   end Change_Direction;


   function Can_Reach_Loop
     (P : Pose;
      Has_Obstacle : Boolean_Array;
      Past_Poses : in out Pose_Sets.Set) return Boolean
   is
      Next_Pose : Pose;
   begin
      case P.Dir is
         when Up =>
            for I in reverse Coordinate'First + 1 .. P.Row loop
               if Has_Obstacle (I - 1, P.Col) then
                  Next_Pose := Pose'(I, P.Col, P.Dir);
                  if Past_Poses.Contains (Next_Pose) then
                     return True;
                  end if;
                  Past_Poses.Insert (Next_Pose);
                  Change_Direction (Next_Pose);
                  exit;
               end if;
            end loop;
         when Down =>
            for I in P.Row .. Coordinate'Last - 1 loop
               if Has_Obstacle (I + 1, P.Col) then
                  Next_Pose := Pose'(I, P.Col, P.Dir);
                  if Past_Poses.Contains (Next_Pose) then
                     return True;
                  end if;
                  Past_Poses.Insert (Next_Pose);
                  Change_Direction (Next_Pose);
                  exit;
               end if;
            end loop;
         when Left =>
            for I in reverse Coordinate'First + 1 .. P.Col loop
               if Has_Obstacle (P.Row, I - 1) then
                  Next_Pose := Pose'(P.Row, I, P.Dir);
                  if Past_Poses.Contains (Next_Pose) then
                     return True;
                  end if;
                  Past_Poses.Insert (Next_Pose);
                  Change_Direction (Next_Pose);
                  exit;
               end if;
            end loop;
         when Right =>
            for I in P.Col .. Coordinate'Last - 1 loop
               if Has_Obstacle (P.Row, I + 1) then
                  Next_Pose := Pose'(P.Row, I, P.Dir);
                  if Past_Poses.Contains (Next_Pose) then
                     return True;
                  end if;
                  Past_Poses.Insert (Next_Pose);
                  Change_Direction (Next_Pose);
                  exit;
               end if;
            end loop;
      end case;

      if Next_Pose.Row = 0 then
         return False;
      else
         return Can_Reach_Loop (Next_Pose, Has_Obstacle, Past_Poses);
      end if;
   end Can_Reach_Loop;


   procedure Move
     (Row, Col : in out Extended_Coordinate;
      Dir : in out Direction)
     with Pre => (Row in Coordinate'Range and Col in Coordinate'Range)
   is
   begin
      case Dir is
         when Up =>
            if Row > Coordinate'First and then Has_Obstacle (Row - 1, Col) then
               Dir := Change_Direction (Dir);
            else
               Row := Row - 1;
               Visit_If_Valid (Row, Col, Visited);
            end if;
         when Down =>
            if Row < Coordinate'Last and then Has_Obstacle (Row + 1, Col) then
               Dir := Change_Direction (Dir);
            else
               Row := Row + 1;
               Visit_If_Valid (Row, Col, Visited);
            end if;
         when Left =>
            if Col > Coordinate'First and then Has_Obstacle (Row, Col - 1) then
               Dir := Change_Direction (Dir);
            else
               Col := Col - 1;
               Visit_If_Valid (Row, Col, Visited);
            end if;
         when Right =>
            if Col < Coordinate'Last and then Has_Obstacle (Row, Col + 1) then
               Dir := Change_Direction (Dir);
            else
               Col := Col + 1;
               Visit_If_Valid (Row, Col, Visited);
            end if;
      end case;
   end Move;

   procedure Try_To_Place_Obstacle_In_Front_Of_Guard
     (Guard_Pose : in out Pose;
      Has_Obstacle : in out Boolean_Array;
      Obstacle_Row, Obstacle_Col : out Extended_Coordinate) is
   begin
      Obstacle_Row := Guard_Pose.Row;
      Obstacle_Col := Guard_Pose.Col;

      case Guard_Pose.Dir is
         when Up => Obstacle_Row := Obstacle_Row - 1;
         when Down => Obstacle_Row := Obstacle_Row + 1;
         when Left => Obstacle_Col := Obstacle_Col - 1;
         when Right => Obstacle_Col := Obstacle_Col + 1;
      end case;

      if Obstacle_Row in Coordinate'Range and then
        Obstacle_Col in Coordinate'Range and then
        not Visited (Obstacle_Row, Obstacle_Col)
      then
         Has_Obstacle (Obstacle_Row, Obstacle_Col) := True;
         Change_Direction (Guard_Pose);
      end if;
   end Try_To_Place_Obstacle_In_Front_Of_Guard;


begin
   Open (File, In_File, Filename);

   declare
      Row, Col : Positive := 1;
   begin
      while not End_Of_File (File) loop
         Get_Line (File, S, S_Last);
         Col := 1;
         while Col <= S_Last loop
            if S (Col) = '#' then
               Has_Obstacle (Row, Col) := True;
            elsif S (Col) = '^' then
               Guard_Col := Col;
               Guard_Row := Row;
               Visit_If_Valid (Guard_Row, Guard_Col, Visited);
            end if;
            Col := Col + 1;
         end loop;
         Row := Row + 1;
      end loop;
   end;

   Close (File);

   while Guard_Row in Coordinate'Range and Guard_Col in Coordinate'Range loop
      declare
         Guard_Pose_New : Pose := Pose'(Guard_Row, Guard_Col, Guard_Dir);
         Has_Obstacle_New : Boolean_Array := Has_Obstacle;
         Obstacle_Row, Obstacle_Col : Extended_Coordinate;
         Obstacle_Was_Placed : Boolean;
         Poses : Pose_Sets.Set (68000, 68001);
      begin
         Try_To_Place_Obstacle_In_Front_Of_Guard
           (Guard_Pose_New, Has_Obstacle_New, Obstacle_Row, Obstacle_Col);
         if Obstacle_Row in Coordinate'Range and then
           Obstacle_Col in Coordinate'Range
         then
            Poses.Insert (Pose'(Guard_Row, Guard_Col, Guard_Dir));
            if Can_Reach_Loop (Guard_Pose_New, Has_Obstacle_New, Poses) then
               Placed_Obstacle (Obstacle_Row, Obstacle_Col) := True;
            end if;
         end if;
      end;
      Move (Guard_Row, Guard_Col, Guard_Dir);
   end loop;

   for I in Coordinate'Range loop
      for J in Coordinate'Range loop
         if Visited (I, J) then
            Total_Visited := Total_Visited + 1;
         end if;
      end loop;
   end loop;

   for I in Coordinate'Range loop
      for J in Coordinate'Range loop
         if Placed_Obstacle (I, J) then
            Total_Obstacles_Placed := Total_Obstacles_Placed + 1;
         end if;
      end loop;
   end loop;

   Put_Line ("Unique Positions Visited: " & Total_Visited'Image);
   Put_Line ("Total Obstacles Placed: " & Total_Obstacles_Placed'Image);

end;

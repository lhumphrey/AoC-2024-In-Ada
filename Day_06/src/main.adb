with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Hashed_Sets;
with Guard_Model;

procedure Main is

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 256);
   S_Last : Natural;
   Row, Col : Natural := 0;
   Total_Visited, Total_Obstacles_Placed : Natural := 0;
   Counter : Natural := 0;

begin
   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, S_Last);
      if S_Last = 0 or else (Col > 0 and then S_Last /= Col) then
         Put_Line ("Error: Empty input or inconsistent number of columns in input.");
         return;
      end if;
      Col := S_Last;
      Row := Row + 1;
   end loop;

   declare
      package This_Guard_Model is new Guard_Model (Row, Col);
      use This_Guard_Model;
   begin

      Reset (File);

      Row := 1;
      while not End_Of_File (File) loop
         Get_Line (File, S, S_Last);
         for I in 1 .. S_Last loop
            if S (I) = '#' then
               Has_Obstacle (Row, I) := True;
            elsif S (I) = '^' then
               Guard_Col := I;
               Guard_Row := Row;
               Visit_If_Valid (Guard_Row, Guard_Col, Visited);
            end if;
         end loop;
         Row := Row + 1;
      end loop;
      Row := Row - 1;

      Close (File);

      while Guard_Row in Row_Coordinate'Range and Guard_Col in Col_Coordinate'Range loop
         declare
            Guard_Pose_New : Pose := Pose'(Guard_Row, Guard_Col, Guard_Dir);
            Has_Obstacle_New : Boolean_Array := Has_Obstacle;
            Obstacle_Row : Extended_Row_Coordinate;
            Obstacle_Col : Extended_Col_Coordinate;
            Obstacle_Was_Placed : Boolean;
            Poses : Pose_Sets.Set (Ada.Containers.Count_Type (Row * Col * 4),
                                   Ada.Containers.Hash_Type (Row * Col * 4 + 1));
         begin
            Try_To_Place_Obstacle_In_Front_Of_Guard
              (Guard_Pose_New, Has_Obstacle_New, Obstacle_Row, Obstacle_Col);
            if Obstacle_Row in Row_Coordinate'Range and then
              Obstacle_Col in Col_Coordinate'Range
            then
               Poses.Insert (Pose'(Guard_Row, Guard_Col, Guard_Dir));
               if Can_Reach_Loop (Guard_Pose_New, Has_Obstacle_New, Poses) then
                  Placed_Obstacle (Obstacle_Row, Obstacle_Col) := True;
               end if;
            end if;
         end;
         Move (Guard_Row, Guard_Col, Guard_Dir);
      end loop;

      for I in Row_Coordinate'Range loop
         for J in Col_Coordinate'Range loop
            if Visited (I, J) then
               Total_Visited := Total_Visited + 1;
            end if;
         end loop;
      end loop;

      for I in Row_Coordinate'Range loop
         for J in Col_Coordinate'Range loop
            if Placed_Obstacle (I, J) then
               Total_Obstacles_Placed := Total_Obstacles_Placed + 1;
            end if;
         end loop;
      end loop;

      Put_Line ("Unique Positions Visited: " & Total_Visited'Image);
      Put_Line ("Total Obstacles Placed: " & Total_Obstacles_Placed'Image);

   end;

end;

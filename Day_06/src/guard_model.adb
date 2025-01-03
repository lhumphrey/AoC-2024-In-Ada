package body Guard_Model is

   procedure Visit_If_Valid
     (Row : in Extended_Row_Coordinate;
      Col : in Extended_Col_Coordinate;
      Visited : in out Boolean_Array) is
   begin
      if Row in Row_Coordinate'Range and Col in Col_Coordinate'Range then
         Visited (Row, Col) := True;
      end if;
   end Visit_If_Valid;
   
   
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
            for I in reverse Row_Coordinate'First + 1 .. P.Row loop
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
            for I in P.Row .. Row_Coordinate'Last - 1 loop
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
            for I in reverse Col_Coordinate'First + 1 .. P.Col loop
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
            for I in P.Col .. Col_Coordinate'Last - 1 loop
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
     (Row : in out Extended_Row_Coordinate;
      Col : in out Extended_Col_Coordinate;
      Dir : in out Direction)
   is
   begin
      case Dir is
         when Up =>
            if Row > Row_Coordinate'First and then Has_Obstacle (Row - 1, Col) then
               Dir := Change_Direction (Dir);
            else
               Row := Row - 1;
               Visit_If_Valid (Row, Col, Visited);
            end if;
         when Down =>
            if Row < Row_Coordinate'Last and then Has_Obstacle (Row + 1, Col) then
               Dir := Change_Direction (Dir);
            else
               Row := Row + 1;
               Visit_If_Valid (Row, Col, Visited);
            end if;
         when Left =>
            if Col > Col_Coordinate'First and then Has_Obstacle (Row, Col - 1) then
               Dir := Change_Direction (Dir);
            else
               Col := Col - 1;
               Visit_If_Valid (Row, Col, Visited);
            end if;
         when Right =>
            if Col < Col_Coordinate'Last and then Has_Obstacle (Row, Col + 1) then
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
      Obstacle_Row : out Extended_Row_Coordinate;
      Obstacle_Col : out Extended_Col_Coordinate) is
   begin
      Obstacle_Row := Guard_Pose.Row;
      Obstacle_Col := Guard_Pose.Col;

      case Guard_Pose.Dir is
         when Up => Obstacle_Row := Obstacle_Row - 1;
         when Down => Obstacle_Row := Obstacle_Row + 1;
         when Left => Obstacle_Col := Obstacle_Col - 1;
         when Right => Obstacle_Col := Obstacle_Col + 1;
      end case;

      if Obstacle_Row in Row_Coordinate'Range and then
        Obstacle_Col in Col_Coordinate'Range and then
        not Visited (Obstacle_Row, Obstacle_Col)
      then
         Has_Obstacle (Obstacle_Row, Obstacle_Col) := True;
         Change_Direction (Guard_Pose);
      end if;
   end Try_To_Place_Obstacle_In_Front_Of_Guard;


end Guard_Model;

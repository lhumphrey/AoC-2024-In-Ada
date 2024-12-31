package body Trails is

   procedure Add_Reachable_Goals 
     (TM : in out Trail_Map; 
      From_Row, From_Col, To_Row, To_Col : Positive) is
   begin
      for E of TM (From_Row, From_Col).Reachable_Goals loop
         if not TM (To_Row, To_Col).Reachable_Goals.Contains (E) then
            TM (To_Row, To_Col).Reachable_Goals.Append (E);
         end if;
      end loop;
   end Add_Reachable_Goals;
   
   procedure Descend (TM : in out Trail_Map; Row, Col : in Positive) is
      Loc : Location := TM (Row, Col);
   begin
      
      if Is_Empty (Loc.Reachable_Goals) or else Loc.Height = 0 then
         return;
      end if;
      
      -- Up
      if Row > TM'First (1) and then TM (Row - 1, Col).Height = Loc.Height - 1 
      then
         Add_Reachable_Goals (TM, Row, Col, Row - 1, Col);
         Descend (TM, Row - 1, Col);
      end if;
      
      -- Down
      if Row < TM'Last (1) and then TM (Row + 1, Col).Height = Loc.Height - 1 
      then
         Add_Reachable_Goals (TM, Row, Col, Row + 1, Col);
         Descend (TM, Row + 1, Col);
      end if;
      
      -- Right
      if Col < TM'Last (2)
        and then TM (Row, Col + 1).Height = Loc.Height - 1 
      then
         Add_Reachable_Goals (TM, Row, Col, Row, Col + 1);
         Descend (TM, Row, Col + 1);
      end if;
      
      -- Left
      if Col > TM'First (2)
        and then TM (Row, Col - 1).Height = Loc.Height - 1 
      then
         Add_Reachable_Goals (TM, Row, Col, Row, Col - 1);
         Descend (TM, Row, Col - 1);
      end if;
      
   end Descend;
   
   function Ascend 
     (TM : in out Trail_Map;
      Row, Col : Positive) return Natural
   is
      Loc : Location := TM (Row, Col);
      Total : Natural := 0;
   begin
      
      if TM (Row, Col).Height = 9 then
         return 1;
      end if;
      
      -- Up
      if Row > TM'First (1) and then TM (Row - 1, Col).Height = Loc.Height + 1 
      then
         Total := Total + Ascend (TM, Row - 1, Col);
      end if;
      
      -- Down
      if Row < TM'Last (1) and then TM (Row + 1, Col).Height = Loc.Height + 1 
      then
         Total := Total + Ascend (TM, Row + 1, Col);
      end if;
      
      -- Right
      if Col < TM'Last (2) and then TM (Row, Col + 1).Height = Loc.Height + 1 
      then
         Total := Total + Ascend (TM, Row, Col + 1);
      end if;
      
      -- Left
      if Col > TM'First (2) and then TM (Row, Col - 1).Height = Loc.Height + 1 
      then
         Total := Total + Ascend (TM, Row, Col - 1);
      end if;
      
      return Total;
      
   end Ascend;

end Trails;

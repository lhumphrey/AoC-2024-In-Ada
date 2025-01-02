package body Gardens is
   
   type Direction is (Up, Down, Left, Right);
   
   
   function Has_Matching_Neighbor (Row, Col : Positive; Dir : Direction) return Boolean is
      Row2 : Natural := (case Dir is when Up => Row - 1, when Down => Row + 1, when others => Row);
      Col2 : Natural := (case Dir is when Left => Col - 1, when Right => Col + 1, when others => Col);
   begin
      if Row2 not in Garden'Range (1) or else Col2 not in Garden'Range (2) then
         return False;
      elsif Puzzle (Row, Col) = Puzzle (Row2, Col2) then
         return True;
      else
         return False;
      end if;
   end Has_Matching_Neighbor;
   
   
   function Is_Side (Row, Col : Positive; Dir : Direction) return Boolean is 
     (case Dir is
        when Up => (Row = Garden'First (1) or else 
                    Garden (Row - 1, Col) /= Garden (Row, Col)),
         when Down => (Row = Garden'Last (1) or else 
                       Garden (Row + 1, Col) /= Garden (Row, Col)),
         when Left => (Col = Garden'First (2) or else 
                       Garden (Row, Col - 1) /= Garden (Row, Col)),
         when Right => (Col = Garden'Last (2) or else 
                        Garden (Row, Col + 1) /= Garden (Row, Col)));
   
   
   function Perimeter (Row, Col : Positive) return Natural is
      Amount : Natural := 0;
   begin
      for Dir in Direction'Range loop
         if not Has_Matching_Neighbor (Row, Col, Dir) then
            Amount := Amount + 1;
         end if;
      end loop;
      return Amount;
   end Perimeter;
   
   
   procedure Process_Plot (Region_Id : Positive; Row, Col : Positive) is
      R : Region := Regions.Element (Region_Id);
   begin
      R.Area := R.Area + 1;
      R.Perimeter := R.Perimeter + Perimeter (Row, Col);
      Regions.Replace_Element (Region_Id, R);
      Processed (Row, Col) := True;
      Garden (Row, Col).Region_Id := Region_Id;
      Garden (Row, Col).Letter := Puzzle (Row, Col);
   end Process_Plot;
   
   
   procedure Cover_Region (Row, Col : Positive) is
      R : Region;
      Region_Id : Positive := Garden (Row, Col).Region_Id;
      I, J : Natural;
   begin
      -- Up
      I := Row - 1;
      J := Col;
      if Row > Puzzle'First (1) and then not Processed (I, J) 
        and then Puzzle (I, J) = Puzzle (Row, Col)
      then
         Process_Plot (Region_Id, I, J);
         Cover_Region (I, J);
      end if;
      
      -- Down
      I := Row + 1;
      if Row < Puzzle'Last (1) and then not Processed (I, J) 
        and then Puzzle (I, J) = Puzzle (Row, Col)
      then
         Process_Plot (Region_Id, I, J);
         Cover_Region (I, J);  
      end if;
      
      -- Left
      I := Row;
      J := Col - 1;
      if Col > Puzzle'First (2) and then not Processed (I, J) 
        and then Puzzle (I, J) = Puzzle (Row, Col)
      then
         Process_Plot (Region_Id, I, J);
         Cover_Region (I, J);   
      end if;
      
      -- Right
      J := Col + 1;
      if Col < Puzzle'Last (2) 
        and then not Processed (I, J) 
        and then Puzzle (I, J) = Puzzle (Row, Col)
      then
         Process_Plot (Region_Id, I, J);
         Cover_Region (I, J);
      end if;
   end Cover_Region;
   
   
   procedure Initialize_Garden is
   begin 
     for I in Garden'Range (1) loop
         for J in Garden'Range (2) loop
            if not Processed (I, J) then
               Regions.Append (Region'(Puzzle (I, J), 1, Perimeter (I, J), 0));
               Garden (I, J) := Plot'(Regions.Last_Index, Puzzle (I, J));
               Processed (I, J) := True;
               Cover_Region (I, J);  
            end if;
         end loop;
      end loop;
   end Initialize_Garden;
   

   procedure Update_Regions_With_Sides is
      R : Region;
   begin
      for I in Garden'Range (1) loop
         for J in Garden'Range (2) loop
            R := Regions.Element (Garden (I, J).Region_Id);
            if Is_Side (I, J, Left) and then
              (not Has_Matching_Neighbor (I, J, Up) or else
                   not Is_Side (I - 1, J, Left))
            then
               R.Sides := R.Sides + 1;
            end if;
            if Is_Side (I, J, Right) and then
              (not Has_Matching_Neighbor (I, J, Up) or else
                   not Is_Side (I - 1, J, Right))
            then
               R.Sides := R.Sides + 1;
            end if;
            if Is_Side (I, J, Down) and then
              (not Has_Matching_Neighbor (I, J, Left) or else 
                   not Is_Side (I, J - 1, Down))
            then
               R.Sides := R.Sides + 1;
            end if;
            if Is_Side (I, J, Up) and then
              (not Has_Matching_Neighbor (I, J, Left) or else
                   not Is_Side (I, J - 1, Up))
            then
               R.Sides := R.Sides + 1;
            end if;
            Regions.Replace_Element (Garden (I, J).Region_Id, R);
         end loop;
      end loop;
   end Update_Regions_With_Sides;
   
   
end Gardens;

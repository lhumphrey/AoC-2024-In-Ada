with Ada.Containers.Vectors;

generic
   Rows : Positive;
   Cols : Positive;
package Gardens is
   
   type Plot is record
      Region_Id : Positive;
      Letter : Character;
   end record;
   
   type Region is record
      Letter : Character;
      Area : Positive;
      Perimeter : Positive;
      Sides : Natural;
   end record;
   
   package Region_Vectors is new Ada.Containers.Vectors (Positive, Region);
   
   Puzzle : array (1 .. Rows, 1 .. Cols) of Character;
   Regions : Region_Vectors.Vector;
   
   procedure Initialize_Garden;
   
   procedure Update_Regions_With_Sides;
   
private
   
   Processed : array (1 .. Rows, 1 .. Cols) of Boolean;
   Garden : array (1 .. Rows, 1 .. Cols) of Plot;

end Gardens;

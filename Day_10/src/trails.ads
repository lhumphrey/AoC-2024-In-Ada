with Ada.Containers.Vectors;

package trails is
   
   Path_Id : Positive := 1;
   
   type Direction is (Up, Down, Left, Right);
   
   type Goal is record
      Row, Col : Natural;
   end record;
   
   package Goal_Vectors is new Ada.Containers.Vectors (Positive, Goal);
   use Goal_Vectors;
   
   type Location is record
      Height : Natural;
      Reachable_Goals : Goal_Vectors.Vector;
   end record;
   
   type Trail_Map is array (Positive range <>, Positive range <>) of Location;

   procedure Descend (TM : in out Trail_Map; Row, Col : Positive);
   
   function Ascend (TM : in out Trail_Map; Row, Col : Positive) return Natural;
   
end trails;

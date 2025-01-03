with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Hashed_Sets;

generic
   Rows : Positive;
   Cols : Positive;
package Guard_Model is

   subtype Row_Coordinate is Integer range 1 .. Rows;
   
   subtype Extended_Row_Coordinate is Integer range 0 .. Rows + 1;
   
   subtype Col_Coordinate is Integer range 1 .. Cols;
   
   subtype Extended_Col_Coordinate is Integer range 0 .. Cols + 1;

   type Direction is (Up, Right, Down, Left);

   type Pose is record
      Row : Extended_Row_Coordinate := 0;
      Col : Extended_Col_Coordinate := 0;
      Dir : Direction;
   end record;
   
   package Pose_Vectors is new Ada.Containers.Vectors (Positive, Pose);
   use Pose_Vectors;

   function Pose_Hash (P : Pose) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type
        (P.Row + P.Col * (Rows + 1) + (Rows * Cols + 1) * (case P.Dir is
            when Up => 1, when Down => 2, when Left => 3, when Right => 4)));

   package Pose_Sets is new
     Ada.Containers.Bounded_Hashed_Sets (Pose, Pose_Hash, "=", "=");
   use Pose_Sets;

   type Boolean_Array is array (Row_Coordinate'Range, Col_Coordinate'Range) of Boolean;
   
   Guard_Dir : Direction := Up;
   Guard_Row : Extended_Row_Coordinate := 0;
   Guard_Col : Extended_Col_Coordinate := 0;
   Visited : Boolean_Array := (others => (others => False));
   Has_Obstacle : Boolean_Array := (others => (others => False));
   Placed_Obstacle : Boolean_Array := (others => (others => False));
   
   procedure Visit_If_Valid
     (Row : in Extended_Row_Coordinate;
      Col : in Extended_Col_Coordinate;
      Visited : in out Boolean_Array);
   
   
   function Change_Direction (D: Direction) return Direction is
     (case D is
         when Up => Right, when Right => Down,
         when Down => Left, when Left => Up);
   
   
   procedure Change_Direction (P : in out Pose);
   
   
   function Can_Reach_Loop
     (P : Pose;
      Has_Obstacle : Boolean_Array;
      Past_Poses : in out Pose_Sets.Set) return Boolean;

   
   procedure Move
     (Row : in out Extended_Row_Coordinate;
      Col : in out Extended_Col_Coordinate;
      Dir : in out Direction)
     with Pre => (Row in Row_Coordinate'Range and Col in Col_Coordinate'Range);
   
   
   procedure Try_To_Place_Obstacle_In_Front_Of_Guard
     (Guard_Pose : in out Pose;
      Has_Obstacle : in out Boolean_Array;
      Obstacle_Row : out Extended_Row_Coordinate;
      Obstacle_Col : out Extended_Col_Coordinate);
   
   
end Guard_Model;

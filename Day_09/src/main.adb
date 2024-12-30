with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Main is

   Num_Blocks : Positive := 19999;

   subtype Block_Index is Natural range 0 .. Num_Blocks - 1;

   subtype File_Id is Natural range 0 .. Block_Index'Last / 2;

   type Block_Size_Array is array (Block_Index) of Natural;

   type Position_Array is array (File_Id) of Natural;

   type Free_Block is record
      Start_Pos : Natural := 0;
      End_Pos : Integer := -1;
   end record;

   function Size_Of (F : Free_Block) return Natural is (F.End_Pos - F.Start_Pos + 1);

   package Free_Block_Vectors is new Ada.Containers.Vectors (Positive, Free_Block);
   use Free_Block_Vectors;

   Puzzle_Filename : String := "input.txt";
   Puzzle_File : File_Type;
   Block_Size : Block_Size_Array;
   Start_Position : Position_Array;
   End_Position : Position_Array;
   Free_Blocks : Free_Block_Vectors.Vector;
   Total_1, Total_2 : Big_Integer := 0;

   function To_File_Id (I : Block_Index) return File_Id is (I / 2);

   function File_Size (I : File_Id) return Natural is
     (End_Position (I) - Start_Position (I) + 1);

   function Free_Space_After (I : File_Id) return Natural
     is
     (Start_Position (I + 1) - End_Position (I) - 1)
       with Pre => (I in File_Id'First .. File_Id'Last - 1);

begin
   -- Read in puzzle input
   Open (Puzzle_File, In_File, Puzzle_Filename);

   declare
      S : String (1 .. Num_Blocks);
      S_Last : Natural;
   begin
      Get_Line (Puzzle_File, S, S_Last);
      for I in 1 .. Num_Blocks loop
         Block_Size (I - 1) := Natural'Value (S (I .. I));
      end loop;
   end;

   Close (Puzzle_File);

   -- Compute file start and end positions and free blocks
   Start_Position (0) := 0;
   End_Position (0) := Block_Size (0) - 1;
   declare
      I : Natural := 2;
      F : Free_Block;
   begin
      while I <= Block_Index'Last loop
         if Block_Size (I - 1) > 0 then
            F.Start_Pos := End_Position (To_File_Id (I - 2)) + 1;
            F.End_Pos := F.Start_Pos + Block_Size (I - 1) - 1;
            Free_Blocks.Append (F);
         end if;
         Start_Position (To_File_Id (I)) :=
           End_Position (To_File_Id (I) - 1) + Block_Size (I - 1) + 1;
         End_Position (To_File_Id (I)) :=
           Start_Position (To_File_Id (I)) + Block_Size (I) - 1;
         I := I + 2;
      end loop;
   end;

   -- Compute solution to Part 1
   declare
      Left_File_Id : File_Id := 0;
      Right_File_Id : File_Id := File_Id'Last;
      Right_File_Remaining : Natural := File_Size (Right_File_Id);
      Free_Space : Natural;
      Position : Natural := 0;
   begin

      while Left_File_Id < Right_File_Id loop
         for I in 1 .. File_Size (Left_File_Id) loop
            Total_1 := Total_1 + To_Big_Integer (Left_File_Id * Position);
            Position := Position + 1;
         end loop;
         Left_File_Id := Left_File_Id + 1;

         if Left_File_Id <= Right_File_Id then
            Free_Space := Free_Space_After (Left_File_Id - 1);
         else
            Free_Space := 0;
         end if;

         for I in 1 .. Free_Space loop
            Total_1 := Total_1 + To_Big_Integer (Right_File_Id * Position);
            Right_File_Remaining := Right_File_Remaining - 1;
            Position := Position + 1;
            if Right_File_Remaining = 0 then
               Right_File_Id := Right_File_Id - 1;
               if Right_File_Id > Left_File_Id then
                  Right_File_Remaining := File_Size (Right_File_Id);
               else
                  Right_File_Remaining := 0;
                  exit;
               end if;
            end if;
         end loop;
      end loop;

      for I in 1 .. Right_File_Remaining loop
         Total_1 := Total_1 + To_Big_Integer (Right_File_Id * Position);
         Position := Position + 1;
      end loop;
   end;

   Put_Line ("Part 1 checksum is: " & Total_1'Image);

   -- Compute solution to Part 2
   declare
      Right_File_Id : Integer := File_Id'Last;
      I : Natural;
      Moved_Block : Boolean := False;
      FB : Free_Block;
   begin
      while Right_File_Id >= 0 loop

         I := Free_Blocks.First_Index;
         while I <= Free_Blocks.Last_Index loop
            FB := Free_Blocks.Element (I);
            if Size_Of (FB) >= File_Size (Right_File_Id) and then
              Start_Position (Right_File_Id) > FB.End_Pos
            then
               for J in FB.Start_Pos .. FB.Start_Pos + File_Size (Right_File_Id) - 1 loop
                  Total_2 := Total_2 + To_Big_Integer (Right_File_Id * J);
               end loop;
               Moved_Block := True;
               FB.Start_Pos := FB.Start_Pos + File_Size (Right_File_Id);
               Free_Blocks.Replace_Element (I, FB);
               exit;
            elsif Start_Position (Right_File_Id) < FB.End_Pos then
               exit;
            end if;
            I := I + 1;
         end loop;

         if Moved_Block then
            if Size_Of (Free_Blocks.Element (I)) = 0 then
               Free_Blocks.Delete (I);
            end if;
         else
            for I in Start_Position (Right_File_Id) .. End_Position (Right_File_Id) loop
               Total_2 := Total_2 + To_Big_Integer (Right_File_Id * I);
            end loop;
         end if;

         Moved_Block := False;
         Right_File_Id := Right_File_Id - 1;
      end loop;

      Put_Line ("Part 2 checksum is: " & Total_2'Image);
   end;

end Main;

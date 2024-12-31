with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 256);
   A : array (1 .. 256, 1 .. 256) of Character := (others => (others => ' '));
   Last_Row, Last_Col : Natural := 0;
   Total1, Total2 : Natural := 0;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) loop
      Get_Line (File, S, Last_Col);
      Last_Row := Last_Row + 1;
      for I in 1 .. Last_Col loop
         A (Last_Row, I) := S (I);
      end loop;
   end loop;

   Close (File);

   for I in A'First(1) .. Last_Row loop
      for J in A'First(2) .. Last_Col loop
         -- Down
         Total1 := Total1 +
           (if I <= A'Last(1) - 3
            and then A(I, J) = 'X'
            and then A(I + 1, J) = 'M'
            and then A(I + 2, J) = 'A'
            and then A(I + 3, J) = 'S' then 1 else 0);
         -- Up
         Total1 := Total1 +
           (if I >= 4
            and then A(I, J) = 'X'
            and then A(I - 1, J) = 'M'
            and then A(I - 2, J) = 'A'
            and then A(I - 3, J) = 'S' then 1 else 0);
         -- Forward
         Total1 := Total1 +
           (if J <= A'Last(2) - 3
            and then A(I, J) = 'X'
            and then A(I, J + 1) = 'M'
            and then A(I, J + 2) = 'A'
            and then A(I, J + 3) = 'S' then 1 else 0);
         -- Backward
         Total1 := Total1 +
           (if J >= 4
            and then A(I, J) = 'X'
            and then A(I, J - 1) = 'M'
            and then A(I, J - 2) = 'A'
            and then A(I, J - 3) = 'S' then 1 else 0);
         -- Up left
         Total1 := Total1 +
           (if I >= 4 and then J >= 4
            and then A(I, J) = 'X'
            and then A(I - 1, J - 1) = 'M'
            and then A(I - 2, J - 2) = 'A'
            and then A(I - 3, J - 3) = 'S' then 1 else 0);
         -- Up Right
         Total1 := Total1 +
           (if I >= 4 and then J <= A'Last(2) - 3
            and then A(I, J) = 'X'
            and then A(I - 1, J + 1) = 'M'
            and then A(I - 2, J + 2) = 'A'
            and then A(I - 3, J + 3) = 'S' then 1 else 0);
         -- Down left
         Total1 := Total1 +
           (if I <= A'Last(1) - 3 and then J >= 4
            and then A(I, J) = 'X'
            and then A(I + 1, J - 1) = 'M'
            and then A(I + 2, J - 2) = 'A'
            and then A(I + 3, J - 3) = 'S' then 1 else 0);
         -- Down Right
         Total1 := Total1 +
           (if I <= A'Last(1) - 3 and then J <= A'Last(2) - 3
            and then A(I, J) = 'X'
            and then A(I + 1, J + 1) = 'M'
            and then A(I + 2, J + 2) = 'A'
            and then A(I + 3, J + 3) = 'S' then 1 else 0);
      end loop;
   end loop;

   Put_Line ("Total 'XMAS's: " & Total1'Image);

   for I in A'First(1) .. Last_Row loop
      for J in A'First(2) .. Last_Col loop
         Total2 := Total2 +
           (if A(I, J) = 'A'
            and then I <= A'Last(1) - 1 and then J <= A'Last(2) - 1
            and then I >= 2 and then J >= 2
            and then ((A(I - 1, J - 1) = 'M' and then A(I + 1, J + 1) = 'S')
              or else (A(I - 1, J - 1) = 'S' and then A(I + 1, J + 1) = 'M'))
            and then ((A(I - 1, J + 1) = 'M' and then A(I + 1, J - 1) = 'S')
              or else (A(I - 1, J + 1) = 'S' and then A(I + 1, J - 1) = 'M'))
            then 1 else 0);
      end loop;
   end loop;

   Put_Line ("Total 'X-MAS's: " & Total2'Image);

end;

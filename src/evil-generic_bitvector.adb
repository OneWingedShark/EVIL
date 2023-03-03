----------------
-- BITVECTOR  --
----------------

Package Body EVIL.Generic_Bitvector with SPARK_Mode => On is
   Function Raise_Last is new EVIL.Generic_Default_Index(
      Result  => Elements,
      Default => Elements'Last
     );

   Function Raise_First is new EVIL.Generic_Default_Index(
      Result  => Elements,
      Default => Elements'First
     );

   Procedure Include( Object : in out Bitvector; Value : in Elements ) is
   Begin
      Object(Value):= True;
   End Include;

   Procedure Exclude( Object : in out Bitvector; Value : in Elements ) is
   Begin
      Object(Value):= False;
   End Exclude;

   Function "OR"(Left, Right : Elements) return Bitvector is
   Begin
      Return Result : Bitvector:= (Others => False) do
         Include(Result, Left );
         Include(Result, Right);
      end return;
   End "OR";

   Function "OR"(Left : Bitvector; Right : Elements) return Bitvector is
   Begin
      Return Result : Bitvector:= Left do
         Include( Result, Right );
      end return;
   End "OR";

   Function "NOT"(Right : Elements) return Bitvector is
   Begin
      Return Result : Bitvector:= (others => True) do
         Exclude( Result, Right );
      end return;
   End "NOT";

   Function Min(Object : Bitvector) return Elements is
   begin
      For Index in Object'Range loop
         if Object(Index) then
            return Index;
         end if;
      end loop;

      return Raise_First;
   end Min;

   Function Max(Object : Bitvector) return Elements is
   begin
      For Index in reverse Object'Range loop
         if Object(Index) then
            return Index;
         end if;
      end loop;

      return Raise_Last;
   end Max;


   Function Succ(Object : Bitvector; Item : Elements) return Elements is
   Begin
      Return Result : Elements:= Elements'First do
         if Item /= Elements'Last then
            SCANNER:
            For Index in Elements'Succ(Item)..Elements'Last loop
               if Object(Index) then
                  Result:= Index;
                  Exit SCANNER;
               end if;
            end loop SCANNER;
         end if;

         Result:= Raise_First;
      end return;
   End Succ;

   -- Returns the previous true item in the vector, or Elements'Last when not found.
   Function Pred(Object : Bitvector; Item : Elements) return Elements is
   Begin
      Return Result : Elements:= Elements'Last do
         if Item /= Elements'First then
            SCANNER:
            For Index in reverse Elements'First..Elements'Pred(Item) loop
               if Object(Index) then
                  Result:= Index;
                  Exit SCANNER;
               end if;
            end loop SCANNER;
         end if;

         Result:= Raise_Last;
      end return;
   End Pred;

   -- NOTE:         with Annotate => (Gnatprove, Terminating)
End EVIL.Generic_Bitvector;

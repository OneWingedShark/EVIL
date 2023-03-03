Pragma Ada_2012;
Pragma Assertion_Policy( Check );

-- EVIL.Generic_Bitvectors
--	This package is to provide a consistent interface for the abstraction of
--	Bitvectors (Arrays with Boolean elements) so that a uniform interface
--	may be presented and operated on.
Generic
   Type Elements is (<>);
Package EVIL.Generic_Bitvector with Pure, SPARK_Mode => On is
   Type Bitvector is Array(Elements) of Boolean
     with Component_Size => 1;

   Function "="(Left : Bitvector; Right : Boolean) return Boolean is
     (for all X of Left => X = Right);

   Procedure Include( Object : in out Bitvector; Value : in Elements )
     with Inline;
   Procedure Exclude( Object : in out Bitvector; Value : in Elements )
     with Inline;

   Function "OR"(Left, Right : Elements) return Bitvector
     with Inline;

   Function "OR"(Left : Bitvector; Right : Elements) return Bitvector
     with Inline;

   Function "NOT"(Right : Elements) return Bitvector
     with Inline;

   -- Returns the minimum value in the vector, or else No_Index/Elements'First.
   Function Min(Object : Bitvector) return Elements;
   -- Returns the maximum value in the vector, or else No_Index/Elements'Last.
   Function Max(Object : Bitvector) return Elements;

   -- Returns the next true item in the vector, or No_Index/Elements'First when not found.
   Function Succ(Object : Bitvector; Item : Elements) return Elements;
   -- Returns the previous true item in the vector, or No_Index/Elements'Last when not found.
   Function Pred(Object : Bitvector; Item : Elements) return Elements;
End EVIL.Generic_Bitvector;

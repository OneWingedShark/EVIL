Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
EVIL.Generic_Bitvector;

Private With
Ada.Unchecked_Conversion;

Package EVIL.Bitvectors with Pure, SPARK_Mode => On is

   -- 2**12 bits -> 4_096-bits (512 Bytes).
   --package Bitvector_12 is new Generic_Bitvector( Int_12 );

   -- 2**6 bits -> 64-bits (8 Bytes).
   package Bitvector_06 is new Generic_Bitvector( Int_06 );

   -- 2**3 bits -> 8-bits (1 Byte).
   package Bitvector_03 is new Generic_Bitvector( Int_03 );

   -- Bitvector/integer conversions.
   Function "+"(X:Bitvector_06.Bitvector) return Interfaces.Unsigned_64
     with Inline, Pure_Function, Convention => Ada;
   Function "+"(X:Interfaces.Unsigned_64) return Bitvector_06.Bitvector
     with Inline, Pure_Function, Convention => Ada;
   Function "+"(X:Bitvector_03.Bitvector) return Interfaces.Unsigned_8
     with Inline, Pure_Function, Convention => Ada;
   Function "+"(X:Interfaces.Unsigned_8) return Bitvector_03.Bitvector
     with Inline, Pure_Function, Convention => Ada;

Private
   function Convert is new Ada.Unchecked_Conversion
     ( Bitvector_06.Bitvector, Interfaces.Unsigned_64 );
   function Convert is new Ada.Unchecked_Conversion
     ( Interfaces.Unsigned_64, Bitvector_06.Bitvector );
   function Convert is new Ada.Unchecked_Conversion
     ( Bitvector_03.Bitvector, Interfaces.Unsigned_8 );
   function Convert is new Ada.Unchecked_Conversion
     ( Interfaces.Unsigned_8, Bitvector_03.Bitvector );

   Function "+"(X:Bitvector_06.Bitvector) return Interfaces.Unsigned_64 is
     ( Convert(X) ) with SPARK_Mode => Off;
   Function "+"(X:Interfaces.Unsigned_64) return Bitvector_06.Bitvector is
     ( Convert(X) ) with SPARK_Mode => Off;
   Function "="(Right, Left : Interfaces.Unsigned_64) return Boolean
     renames Interfaces."=";
   Function "+"(X:Bitvector_03.Bitvector) return Interfaces.Unsigned_8 is
     ( Convert(X) ) with SPARK_Mode => Off;
   Function "+"(X:Interfaces.Unsigned_8) return Bitvector_03.Bitvector is
     ( Convert(X) ) with SPARK_Mode => Off;
   Function "="(Right, Left : Interfaces.Unsigned_8) return Boolean
     renames Interfaces."=";

End EVIL.Bitvectors;

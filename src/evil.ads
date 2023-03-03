Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Interfaces;

Use
Interfaces;

-- Everyone's Verified & Independent Library.
Package EVIL with Pure, SPARK_Mode => On is
   USE_EXCEPTIONS : Constant Boolean;

   -- A 64-bit IEEE floating-point number, without the non-numeric values.
   subtype Real is Interfaces.IEEE_Float_64 range Interfaces.IEEE_Float_64'Range;

   -- Renaming the Interfaces [unsigned] Integers.
   Subtype Int_64 is Interfaces.Unsigned_64;
   Subtype Int_32 is Interfaces.Unsigned_32;
   Subtype Int_16 is Interfaces.Unsigned_16;
   Subtype Int_08 is Interfaces.Unsigned_8;

   -- Defining integer types for 48-, 24-, 6-, and 3-bit Integers.
   -- NOTE: 48- and 24- bit integers are used in discrete signal-processing, and
   -- 6- and 3- bit integers can be used to index 64-bit and 8-bit bitvectors.
   Type Int_48 is range 0..2**48-1 with Size => 48;
   Type Int_24 is range 0..2**24-1 with Size => 24;
   Type Int_12 is range 0..2**12-1 with Size => 12;
   Type Int_06 is range 0..2**06-1 with Size =>  6;
   Type Int_03 is range 0..2**03-1 with Size =>  3;

   -- Returns the HIGH and LOW half of the given integer's bits.
   Function High( X : Int_06 ) return Int_03 with Inline, Pure_Function;
   Function Low ( X : Int_06 ) return Int_03 with Inline, Pure_Function;
   Function High( X : Int_12 ) return Int_06 with Inline, Pure_Function;
   Function Low ( X : Int_12 ) return Int_06 with Inline, Pure_Function;
   Function High( X : Int_24 ) return Int_12 with Inline, Pure_Function;
   Function Low ( X : Int_24 ) return Int_12 with Inline, Pure_Function;
   Function High( X : Int_48 ) return Int_24 with Inline, Pure_Function;
   Function Low ( X : Int_48 ) return Int_24 with Inline, Pure_Function;

   -- Given the HIGH and LOW halves of an integer, return the full integer.
   Function "**"(Left, Right : Int_03) return Int_06 with Inline, Pure_Function;
   Function "**"(Left, Right : Int_06) return Int_12 with Inline, Pure_Function;
   Function "**"(Left, Right : Int_12) return Int_24 with Inline, Pure_Function;
   Function "**"(Left, Right : Int_24) return Int_48 with Inline, Pure_Function;


   -- An enumeration for [sub]trees, for optimizing representation space-wise.
   Type Tree_State is (Empty, Single, Partial, Full)
     with Size => 2;


   No_Index       : Exception;
Private
   USE_EXCEPTIONS : Constant Boolean:= False;

End EVIL;

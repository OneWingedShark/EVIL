With
Ada.Unchecked_Conversion;

Package Body EVIL with SPARK_Mode => On is


   ---------------
   --  INDICES  --
   ---------------
   --  These records are the decomposition of a number into two halves, this is
   --  to facilitate a divide-and-conquor strategy like the VAN EMDE BOAS tree,
   --  as well as provide uniformity of such manipulations -- some thought was
   --  given to make these congurent with the byte-layout of the computer (e.g)
   --  little-endian, value 16#CAFE_BABE# stored as 16#BEBA_FECA#, but this was
   --  ultimately decided against.

   Type Indices_06 is record
      Lo, Hi : Int_03;
   end record
   with Pack, Size => Int_06'Size;

   Type Indices_12 is record
      Lo, Hi : Int_06;
   end record
   with Pack, Size => Int_12'Size;

   Type Indices_24 is record
      Lo, Hi : Int_12;
   end record
   with Pack, Size => Int_24'Size;

   Type Indices_48 is record
      Lo, Hi : Int_24;
   end record
   with Pack, Size => Int_48'Size;

   -- Convert an Integer to a record of two integers of half the size.
   Function To_Record is new Ada.Unchecked_Conversion
     (Source => Int_48, Target => Indices_48);
   Function To_Record is new Ada.Unchecked_Conversion
     (Source => Int_24, Target => Indices_24);
   Function To_Record is new Ada.Unchecked_Conversion
     (Source => Int_12, Target => Indices_12);
   Function To_Record is new Ada.Unchecked_Conversion
     (Source => Int_06, Target => Indices_06);

   -- Convert a record of two Integers to an Integer of twice the length.
   Function To_Index is new Ada.Unchecked_Conversion
     (Source => Indices_48, Target => Int_48);
   Function To_Index is new Ada.Unchecked_Conversion
     (Source => Indices_24, Target => Int_24);
   Function To_Index is new Ada.Unchecked_Conversion
     (Source => Indices_12, Target => Int_12);
   Function To_Index is new Ada.Unchecked_Conversion
     (Source => Indices_06, Target => Int_06);

   -- Return the HIGH/LOW half of a given Integer.
   Function High( X : Int_06 ) return Int_03 is (To_Record(X).Hi);
   Function Low ( X : Int_06 ) return Int_03 is (To_Record(X).Lo);
   Function High( X : Int_12 ) return Int_06 is (To_Record(X).Hi);
   Function Low ( X : Int_12 ) return Int_06 is (To_Record(X).Lo);
   Function High( X : Int_24 ) return Int_12 is (To_Record(X).Hi);
   Function Low ( X : Int_24 ) return Int_12 is (To_Record(X).Lo);
   Function High( X : Int_48 ) return Int_24 is (To_Record(X).Hi);
   Function Low ( X : Int_48 ) return Int_24 is (To_Record(X).Lo);

   -- Construct an Integer given two Integers of half the size.
   Function "**"(Left, Right : Int_03) return Int_06 is
     (To_Index((Lo => Left, Hi =>  Right)));
   Function "**"(Left, Right : Int_06) return Int_12 is
     (To_Index((Lo => Left, Hi =>  Right)));
   Function "**"(Left, Right : Int_12) return Int_24 is
     (To_Index((Lo => Left, Hi =>  Right)));
   Function "**"(Left, Right : Int_24) return Int_48 is
     (To_Index((Lo => Left, Hi =>  Right)));

End EVIL;

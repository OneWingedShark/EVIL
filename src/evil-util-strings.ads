Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Ada.Strings;

-- EVIL.Util.Strings
--	This package is to provide a consistent interface for the abstraction of
--	'Strings' and Arrays so that a uniform interface may be presented
--	regardless of the element type (e.g. CHARACTER vs WIDE_WIDE_CHARACTER).
Generic
   Type Character is (<>);
   Type String is array(Positive Range <>) of Character;
   Space        : Character;
   Empty_String : String := (2..1 => <>);
Package EVIL.Util.Strings with Pure, SPARK_Mode => On is

   -- Ada.Strings definition for ALIGNMENT:
   --	type Alignment  is (Left, Right, Center);
   Subtype Alignemnt is Ada.Strings.Alignment;

   -- Ada.Strings definition for TRUNCATION:
   --	type Truncation is (Left, Right, Error);
   Subtype Truncation is Ada.Strings.Truncation;

   -- Ada.Strings definition for MEMBERSHIP:
   --	type Membership is (Inside, Outside);
   Subtype Membership is Ada.Strings.Membership;

   -- Ada.Strings definition for DIRECTION:
   --	type Direction  is (Forward, Backward);
   Subtype Direction is Ada.Strings.Direction;

   -- Ada.Strings definition for TRIM_END:
   --	type Trim_End   is (Left, Right, Both);
   Subtype Trim_End is Ada.Strings.Trim_End;

--   Ada.Strings.
--    Length_Error, Pattern_Error, Index_Error, Translation_Error : exception;


   -- Removes the given character from the head of the string.
   Function Trim_Left( S : String; Ch : Character := Space ) return String
     with Global => Null, Depends => (Trim_Left'Result => (S, Ch)),
       Post => (if Trim_Left'Result'Length in Positive
                  then Trim_Left'Result(Trim_Left'Result'First) /= Ch);

   -- Removes the given character from the tail of the string.
   Function Trim_Right( S : String; Ch : Character := Space ) return String
     with Annotate => (gnatprove, Terminating),
       Post => (if Trim_Right'Result'Length in Positive
                     then Trim_Right'Result(Trim_Right'Result'Last) /= Ch);

   -- Removes the given character from both ends of the given string.
   Function Trim( S : String; Ch : Character := Space ) return String
     with Post => (if Trim'Result'Length in Positive
                     then Trim'Result(Trim'Result'First) /= Ch
                      and Trim'Result(Trim'Result'Last)  /= Ch );

   -- Removes from the head of INPUT the elemeents of ITEMS.
   Function Trim_Left( Input, Items : String ) return String
     with Post => (if Trim_Left'Result'Length in Positive then
                     (for all Ch of Items =>
                          Trim_Left'Result(Trim_Left'Result'First) /= Ch));

   -- Removes from the tail of INPUT the elemeents of ITEMS.
   Function Trim_Right( Input, Items : String ) return String
     with Post => (if Trim_Right'Result'Length in Positive then
                     (for all Ch of Items =>
                          Trim_Right'Result(Trim_Right'Result'Last) /= Ch));

   -- Removes from both ends of INPUT the elemeents of ITEMS.
   Function Trim( Input, Items : String ) return String
     with Post => (if Trim'Result'Length in Positive then
                     (for all Ch of Items =>
                          Trim'Result(Trim'Result'First) /= Ch
                      and Trim'Result(Trim'Result'Last)  /= Ch ));

   Generic
      with Function "="(Left, Right :    String) return Boolean is <>;
   Package Search_and_Replace is

      -- Finds location of the first item of the given element.
      Function Index( Input : String; Item : Character ) return Natural
        with Post => (if (for some C of Input => C = Item)
                      then Index'Result in Positive
                       and Input(Index'Result) = Item
                      else Index'Result = 0);

      -- Finds location of the first item of the given element.
      Function Index( Input, Item : String ) return Natural
           with Post => (if (for some X in Input'First..1+Input'Last-Item'Length
                               => Input(X..X+Item'Length-1) = Item)
                         then Index'Result in Positive
                         else Index'Result = 0);

      --
      Function Replace( Object, Pattern, Replacement : String ) return String;
      --       with Post => (

      Function Replace_All( Object, Pattern, Replacement : String ) return String;

   End Search_and_Replace;


   Pragma Annotate( Trim,	Terminates );
   Pragma Annotate( Trim_Left,	Terminates );
   Pragma Annotate( Trim_Right,	Terminates );
End EVIL.Util.Strings;

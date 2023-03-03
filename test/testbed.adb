With
Testbed.Instantiations.Strings,
Ada.Strings;

Package Body Testbed is

    Function Pass_Fail( Input : Boolean ) return String is
      (if Input then "Pass" else "Fail");

   Function Trim_Left(Item : String; Ch : Character) Return String
     renames Testbed.Instantiations.Strings.Trim_Left;
--        with Pure_Function, Inline,
--             Post =>  Trim_Left'Result'Length <= Item'Length and
--                     (Trim_Left'Result'Length = 0 or else
--                      Trim_Left'Result(Trim_Left'Result'First) /= Ch);
--
--
--      Function Trim_Left(Item : String; Ch : Character) Return String is
--      Begin
--  	if Item'Length not in Positive then
--  	    Return "";
--  	else
--  	    Declare
--  		Head : Character renames Item( Item'First );
--  		Subtype Tail is Positive range Positive'Succ(Item'First)..Item'Last;
--  	    Begin
--  		Return (if Head = Ch then Trim_Left(Item(Tail), Ch) else Item);
--  	    End;
--  	end if;
--      End Trim_Left;


   Function Image(Item : Natural) Return String is
   Begin
      Return Trim_Left(Natural'Image(Item), Ada.Strings.Space);
   End Image;


    Function Bracket( Index, Total : Natural ) return String is
    Begin
	Return '[' & Image(Index) & " of " & Image(Total) & ']';
    End Bracket;


End Testbed;

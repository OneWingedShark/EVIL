With
Ada.Exceptions.Traceback,

Ada.Strings.Fixed,
Ada.Integer_Text_IO,
Ada.Text_IO;

Package Body Testbed.Types is
    Function Name(Object: Node) Return String is
      (if Object.Has_Name then +Object.Name_Part else "");

    Function Make( Name : String  ) return Node is
      (Has_Name => True, Has_Functor => False, Func_Part => <>, Name_Part => +Name);

    Function Make( Name : String;
		   Fn   : Functor ) return Node is
      (Has_Name => True,  Has_Functor => True, Func_Part => (True,Fn), Name_Part => +Name);
    Function Make( Fn   : Functor ) return Node is
      (Has_Name => False, Has_Functor => True, Func_Part => (True,Fn), Name_Part => <>);

    Function Create return Test is
      (Test_Tree.Empty_Tree with null record);

--      Function  Run(
--         Object  : in    Test;
--         Verbose :       Boolean      := True
--        ) return Boolean is
--      Begin
--  	Object.Iterate_Children
--      End Run;

    Function Run(
       Object  : in    Node;
       Verbose :       Boolean      := True
      ) return Boolean is
    Begin
	Return Result : Constant Boolean :=
	  (if Object.Has_Functor then Object.Func_Part.Fn.all) do
	    if Verbose then
		Ada.Text_IO.Put( Pass_Fail(Result) & '.');
	    end if;
	end return;
    End Run;

    -- Runs the item at the specified cursor.
    Generic
	Verbose : Boolean := True;
    Function  Cursor_Run(
       Object  : in    Test_Tree.Cursor
      ) return Boolean;

    Function  Cursor_Run(
       Object  : in    Test_Tree.Cursor
      ) return Boolean is
	use Ada.Containers;

	-- Returns the number of nodes at the given item's level.
	Function Level_Count return Natural is
	  ( Natural(Test_Tree.Child_Count( Test_Tree.Parent(Object) )) );

	-- Returns the position of the given item among the nodes ar its level.
	Function Level_Position return Positive is
	    Use Test_Tree;
	Begin
	    Return Result : Positive := 1 do
		Declare
		    Found : Boolean := False;
		    procedure Count (Position : Cursor) is
		    begin
			Found:= Found OR Position = Object;
			Result := Result + (if Found then 0 else 1);
		    end Count;
		Begin
		    Iterate_Children(Parent(Object), Count'Access);
		End;
	    End return;
	End Level_Position;


    Begin
	Return Result : Boolean := True do
	    if not Test_Tree.Has_Element(Object) then return; end if;

	    Declare
		Leaf  : Constant Boolean := Test_Tree.Is_Leaf( Object );
		Item  : Constant Node    := Test_Tree.Element( Object );
		Nest  : Constant Natural := Natural(Count_Type'Pred( Test_Tree.Depth(Object) ));

		Total : Constant Positive := Level_Count;
		Index : Constant Positive := Level_Position;
	    Begin
		if Item.Has_Functor then
		    Result := Item.Func_Part.Fn.All;
		end if;

		if Verbose then
		    Ada.Text_IO.Put( (1..Nest=> '.') & Bracket(Index, Total) & ASCII.HT );
		    if Item.Has_Name then
			Ada.Text_IO.Put( Name(Item) );
		    else
			Ada.Text_IO.Put( "(Anonymous)" );
		    end if;

		    Ada.Text_IO.Put( ASCII.HT );
		    Ada.Text_IO.Put_Line( Pass_Fail(Result) & '.' );
		end if;
	    End;
	End return;
    End Cursor_Run;


    Overriding
    Function  Run(
       Object  : in    Test;
       Verbose :       Boolean      := True
      ) return Boolean is
	use Test_Tree;

	Generic
	    Result : in out Boolean;
	    with Function Run (Object  : in Cursor) return Boolean is <>;
	Procedure Run_Function(Object  : in Cursor);
	Procedure Run_Function(Object  : in Cursor) is
	Begin
	    Result := Result AND Run(Object);
	End Run_Function;


	Function Run is new Cursor_Run(Verbose);
    Begin
	Ada.Text_IO.Put_Line( "Root." );
	Return Result : Boolean := True do
	    Declare
		Procedure Run is new Run_Function( Result => Result );
	    Begin
		Iterate_Subtree( Object.Root, Process => Run'Access );
	    End;
	End return;
    End Run;


    Overriding
    Procedure Add(
       Object : in out Test;
       Name   : in     String;
       Set    : in     Functor_Set
      ) is
	Position : Test_Tree.Cursor;
    Begin
	-- Inserts a name-node...
	Object.Insert_Child(
		     Parent   => Object.Root,
		     Before   => Test_Tree.No_Element,
		     Position => Position,
		     New_Item => Make(Name)
		    );

	-- and inserts the set of tests under it.
	For Fn of Set loop
	    Object.Append_Child(
		     Parent   => Position,
		     New_Item => Make(Fn)
		    );
	End loop;
    End Add;

    Overriding
    Procedure Add(
       Object : in out Test;
       Name   : in     String;
       Fn     : in     Functor
      ) is
    Begin
	Object.Insert_Child(
		     Parent   => Object.Root,
		     Before   => Test_Tree.No_Element,
		     New_Item => Make(Name,Fn => Fn)
		    );
    End Add;


    Overriding
    Procedure Add(
       Object    : in out Test;
       Name      : in     String;
       Collection: in     Test--_Interface'Class
      ) is
--  	Current : Test_Tree.Cursor;
--Subtree  : Test := Collection;
    Begin
	Ada.Text_IO.Put_Line( "Inserting Subtree." );
	-- Inserts a name-node...

	Test_Tree.Tree(Object).Insert_Child(
		     Parent   => Object.Root,
		     Before   => Test_Tree.No_Element,
--  		     Position => Current,
		     New_Item => Make(Name)
		    );

--  	Object.Splice_Subtree(
--      Parent   => Position,
--      Before   => Test_Tree.No_Element,
--      Source   => Subtree,
--      Position => Position
--     );
	Ada.Text_IO.Put_Line( "Finished Subtree." );
    exception
	when E : Others => null;
--  	    Declare
--  		ID   : Constant Ada.Exceptions.Exception_Id := Ada.Exceptions.Exception_Identity(E);
--  		Name : Constant String := Ada.Exceptions.Exception_Name(ID);
--  		Mssg : Constant String := Ada.Exceptions.Exception_Message(E);
--  	    Begin
--  		Ada.Text_IO.Put_Line( Name & ASCII.HT & Mssg );
--  		Ada.Text_IO.Flush;
--  	    End;
    End Add;



--  --      Use Ada.Text_IO, Ada.Integer_Text_IO;
--  --
--  --      Package ITO renames Ada.Integer_Text_IO;
--  --
--  --      Procedure Run_Tests( Object : Test_Store.Vector ) is
--  --  	Last  : Constant Natural := Natural(Object.Length);
--  --  	subtype Index_Range is Natural range 1..Last;
--  --  	Width : Natural renames Index_Range'Width;
--  --  	Count : String(1..Width);
--  --  	Current : String(1..Width);
--  --  	Use Test_Store;
--  --      Begin
--  --  	Put(Count, Index_Range'Last);
--  --  	For Item in Index_Range loop
--  --  	    Put(Current, Item);
--  --  	    Put(
--  --  	        "Test " & Current & " of " & Count & ": " &
--  --  	        (if Object(Item).All then "Pass" else "Fail") & '.'
--  --  	     );
--  --  	end loop;
--  --      End Run_Tests;
--  --
--  --      Procedure Run_Tests( Object : Test_Units.Map ) is
--  --  	Use Test_Units;
--  --      Begin
--  --  	For Item in Object.Iterate loop
--  --  	    Put_Line ( Key(Item) );
--  --  	    Run_Tests( Element(Item) );
--  --  	End loop;
--  --      End Run_Tests;
    Function "+"( Input : String ) return NP is
      ( Name => Name_Holder.To_Holder(Input), Exists => True );

    Function "+"( Input : NP ) return String is
      (if Input.Exists then Input.Name.Element else "");



End Testbed.Types;

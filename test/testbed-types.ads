With
Ada.Containers.Indefinite_Multiway_Trees,
Ada.Containers.Indefinite_Holders,
Ada.Containers.Indefinite_Vectors,
Ada.Containers.Indefinite_Ordered_Maps;

Package Testbed.Types with Elaborate_Body is
    Function Dummy      return Boolean is (True);
    Function Dummy_Fail return Boolean is (False);

    Use Ada.Containers;


    -- A test-node consits of either a set of tests or a singular test.

    Type Test(<>) is new Test_Interface with private;
    Function Create return Test;

    Overriding
    Procedure Add(
       Object : in out Test;
       Name   : in     String;
       Set    : in     Functor_Set
      );

    Overriding
    Procedure Add(
       Object     : in out Test;
       Name       : in     String;
       Collection : in     Test --_Interface'Class
      );

    Procedure Add(
       Object : in out Test;
       Name   : in     String;
       Fn     : in     Functor
      );

    Overriding
    Function  Run(
       Object  : in    Test;
       Verbose :       Boolean      := True
      ) return Boolean;


    Type Node(<>) is private;
Private
    Package Name_Holder  is new Indefinite_Holders(String);

    Type NP( Exists : Boolean ) is record
	Case Exists is
	When True  => Name : Name_Holder.Holder;
	When False => Null;
	end case;
    end record;
    Function "+"( Input : String ) return NP;
    Function "+"( Input : NP ) return String;

    Type FP( Exists : Boolean ) is record
	Case Exists is
	When True  => Fn : Functor;
	When False => Null;
	end case;
    end record;


    Function Run(
       Object  : in    Node;
       Verbose :       Boolean      := True
      ) return Boolean;

    Function Name(Object: Node) Return String;
    Function Make( Name : String  ) return Node;
    Function Make( Name : String;
		   Fn   : Functor ) return Node;
    Function Make( Fn   : Functor ) return Node;
    Type Node( Has_Name, Has_Functor : Boolean ) is record
	Name_Part : NP( Exists => Has_Name    );
	Func_Part : FP( Exists => Has_Functor );
    end record
      with
	Type_Invariant => Has_Functor or Has_Name;

    Package Test_Tree is new Indefinite_Multiway_Trees ( Node );



    Type Test is new Test_Tree.Tree and Test_Interface with null record;



--      Type Test is new Abstract_Test with record
--  	Name    : Name_Holder.Holder;
--  	Test_Set: Functor_Set.Map;
--      end record;
--
--
--
--  --      Type Test is not null Access Function Return Boolean;
--  --
--  --      Package Test_Store is new Indefinite_Vectors(
--  --         Index_Type   => Positive,
--  --         Element_Type => Test
--  --        );
--  --
--  --
--  --      Package Test_Units is new Indefinite_Ordered_Maps(
--  --         "="          => Test_Store."=",
--  --         Key_Type     => String,
--  --         Element_Type => Test_Store.Vector
--  --        );
--  --
--  --      Procedure Run_Tests( Object : Test_Store.Vector );
--  --      Procedure Run_Tests( Object : Test_Units.Map    );

End Testbed.Types;

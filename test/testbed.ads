Package Testbed with Pure is
    Type Functor      is not null access Function return Boolean;
    Type Functor_Set  is Array (Positive range <>) of Functor;

    Type Result_Set   is Array (Positive range <>) of Boolean
      with Component_Size => 1, Default_Component_Value => False;


    -- Prints pass/fail message.
    Function Pass_Fail( Input : Boolean ) return String
    with Inline;

    -- Trimmed image-string.
    Function Image(Item : Natural) Return String
      with Inline, Post => Item = Natural'Value(image'Result),
           Depends => (Image'Result => Item);

    -- Returns string in the form of "[### of ###]".
    Function Bracket( Index, Total : Natural ) return String
      with Inline;


    Type Test_Interface is Interface;
    Procedure Add(
       Object : in out Test_Interface;
       Name   : in     String;
       Set    : in     Functor_Set
      ) is abstract;

    Procedure Add(
       Object : in out Test_Interface;
       Name   : in     String;
       Fn     : in     Functor
      ) is abstract;

    Procedure Add(
       Object     : in out Test_Interface;
       Name       : in     String;
       Collection : in     Test_Interface--'Class
      ) is abstract;


--      Function  Name(
--         Object  : in    Test_Interface
--        ) Return String is abstract;

--      Function  Run(
--         Object  : in    Test_Interface
--        ) return Result_Set is abstract;


    Function  Run(
       Object  : in    Test_Interface;
       Verbose :       Boolean      := True
      ) return Boolean is abstract;

End Testbed;

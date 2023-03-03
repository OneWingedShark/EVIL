With
Ada.Containers.Indefinite_Holders,
env_vec;

Package Body Env is
   Package Env_Class is new Ada.Containers.Indefinite_Holders(
--        "="          => ,
      Element_Type => Environment_Class
     );


   Type Env_Data is tagged record
      Sub_Environments : env_vec.Vector;
   end record;

   Type Data_Record( Value : Data_Type ) is record
      Case Value is
         When dt_Text    => Null; --String_holder;
         When dt_Integer => Integer_Value : Integer;
         When dt_Real    => Real_Value    : Float;
         When dt_Env     => Environment   : Env_Class.Holder;
      end case;
   end record;


End Env;

With
Ada.Strings.Less_Case_Insensitive,
Ada.Containers.Indefinite_Ordered_Maps;

Package Env is
      Type Environment(<>) is tagged private;

   Subtype Environment_Class is Environment'Class;

--     Package Environment_Vector is new Ada.Containers.Indefinite_Vectors(
--  --      "="          => ,
--        Index_Type   => Positive,
--        Element_Type => Environment_Class
--       );

Private
   Type Env_Data is tagged;

   Type Data_Type is ( dt_Text, dt_Integer, dt_Real, dt_Env );
   Type Data_Record( Value : Data_Type );



--     Package Environment_Association is new Ada.Containers.Indefinite_Ordered_Maps
--       (
--        "<"          => Ada.Strings.Less_Case_Insensitive,
--  --        "="          => ,
--        Key_Type     => String,
--        Element_Type => Data_Record
--       );



   Type Environment( Data : not null access Env_Data ) is tagged record
Null;--      Sub_Environments : Environment_Vector.Vector;
   end record;




End Env;

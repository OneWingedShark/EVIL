With
Ada.Containers.Indefinite_Vectors;

With
Env;

Package env_vec is new Ada.Containers.Indefinite_Vectors(
      "="          => Env."=",
      Index_Type   => Positive,
      Element_Type => Env.Environment_Class
     );

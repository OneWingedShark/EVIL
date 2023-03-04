Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Ada.Unchecked_Conversion;

Package Body EVIL.vEB with SPARK_Mode => On is
   Generic
      Type Element is private;
      Type Index   is (<>);
      Type Vector  is array( Index ) of Element;
      Default : Element;
   Package Generic_Unconstrain is
      Type Unconstrained_Array is Array(Index range <>) of Element;

      Function "+"( Right : Vector ) return Unconstrained_Array is
        ( Unconstrained_Array( Right ) );
      Function "+"( Right : Unconstrained_Array ) return Vector;
   End Generic_Unconstrain;
   Package Body Generic_Unconstrain is
      Function "+"( Right : Unconstrained_Array ) return Vector is
      Begin
         Return Result : Vector:= (others => Default) do
            declare
               Subtype Constrained is Unconstrained_Array(Right'Range);
               Slice : Constrained
                 with Import, Address => Result(Right'First)'Address;
            begin
               Slice:= Right;
            end;
         end return;
      End "+";
   End Generic_Unconstrain;

   function Convert is new
     Ada.Unchecked_Conversion( Interfaces.Unsigned_8, VEB_Byte );



   Package Body VEB_Construction is
      Function DEBUG_IMAGE ( Object : in Galactic_Cluster ) return String is
         package Unconstrained_Cluster is new Generic_Unconstrain
           ( Subtree, Galaxy, Galactic_Cluster, Galaxy_VEB.Create );
         use Unconstrained_Cluster;
         Function Img( X : Unconstrained_Array ) return String is
           (case X'Length is
               when 0 => "",
               when 1 => Galaxy_VEB.DEBUG_IMAGE( X(X'Last) ),
               when others =>
                  Img( X(X'First..X'First) ) &','&
              Img( X(Galaxy'Succ(X'First)..X'Last) )
           );
      Begin
         Return Img( +Object );
      End DEBUG_IMAGE;


      Function DEBUG_IMAGE ( Object : in Universe_VEB ) return String is
        ('(' &
         (case Object.State is
             when Full    => "F:*",
             when Empty   => "E: ",
             when Single  => "S:" & Object.Value'Image, -- DEBUG_IMAGE( (others => false) or Object.Value ),
             when Partial => "P:" & DEBUG_IMAGE( Object.Data )
         ) & ')'
        );

      Function Create   (Is_Full : Boolean:= False) return Universe_VEB is
        (if Is_Full
         then Universe_VEB'( State => Full  )
         else Universe_VEB'( State => Empty )
        );

      Function State_Of ( Object : in Universe_VEB ) return Tree_State is
        ( Object.State );


      Function Is_Empty ( Object : in     Universe_VEB ) return Boolean is
        (Object.State = Empty);


      Function Is_Full  ( Object : in     Universe_VEB ) return Boolean is
        (Object.State = Full);


      Function Contains ( Object : in     Universe_VEB;
                          Key    : in     Universe ) return Boolean is
        (case Object.State is
            when Full    => True,
            when Empty   => False,
            when Single  => Object.Value = Key,
            when Partial => Galaxy_VEB.Contains(Object.Data(High(Key)), Low(Key))
        );


      Function Min      ( Object : in     Universe_VEB ) return Universe is
        (case Object.State is
            when Full    => Universe'First,
            when Empty   => Raise No_Index,
            when Single  => Object.Value,
            when Partial => Object.Minimum
        );


      Function Max      ( Object : in     Universe_VEB ) return Universe is
        (case Object.State is
            when Full    => Universe'First,
            when Empty   => Raise No_Index,
            when Single  => Object.Value,
            when Partial => Object.Maximum
        );


      Function First    ( Object : in     Universe_VEB ) return Universe is
         Use Galaxy_VEB;
      Begin
         Return Result : Universe:= Universe'First do
            case Object.State is
               when Full -- FULL should never happen; and EMPTY doesn't matter.
                  | Empty   => return;
               when Single  =>
                  if Object.Value = Universe'First then
                     Result:= Universe'Succ( Result );
                  end if;
               when Partial =>
                  SCANNER:
                  For Index in Object.Data'Range loop
                     case State_Of( Object.Data(Index) ) is
                        when Full    => Null;
                        when Empty   =>
                           Result:= Index ** Galaxy'First;
                           Exit Scanner;
                        when Single  =>
                           Result:= Index **
                             (if Min( Object.Data(Index) ) /= Galaxy'First
                              then Galaxy'First
                              else Galaxy'Succ(Galaxy'First)
                             );
                           Exit Scanner;
                        when Partial =>
                           Result:= Index ** First(Object.Data(Index));
                           exit SCANNER;
                     end case;
                  end loop SCANNER;
            end case;
         end return;
      End First;


      Function Last     ( Object : in     Universe_VEB ) return Universe is
         Use Galaxy_VEB;
      Begin
         Return Result : Universe:= Universe'Last do
            case Object.State is
               when Full -- FULL should never happen; and EMPTY doesn't matter.
                  | Empty   => return;
               when Single  =>
                  if Object.Value = Universe'Last then
                     Result:= Universe'Pred( Result );
                  end if;
               when Partial =>
                  SCANNER:
                  For Index in reverse Object.Data'Range loop
                     case State_Of( Object.Data(Index) ) is
                        when Full    => Null;
                        when Empty   => Exit Scanner;
                        when Single  =>
                           Result:= Index **
                             (if Max( Object.Data(Index) ) /= Galaxy'Last
                              then Galaxy'Last
                              else Galaxy'Pred(Galaxy'Last)
                             );
                           Exit Scanner;
                        when Partial =>
                           Result:= Index ** Last(Object.Data(Index));
                           exit SCANNER;
                     end case;
                  end loop SCANNER;
            end case;
         end return;
      End Last;

      Function Succ     ( Object : in     Universe_VEB;
                          Key    : in     Universe     ) return Universe is
         Function Succ( System : Galaxy:= High(Key);
                        Planet : Galaxy:= Low(Key)
                       ) return Universe is
            Use Galaxy_VEB;
         Begin
            Return Result : Universe:=
              System ** Succ( Object.Data(System), Planet );
         Exception
            when No_Index =>
               -- If we didn't find a successor in the the GALAXY subtree, then
               -- we need to search the next GALAXY, but we also need to keep in
               -- mind that the successor *IS* the first element if that GALAXY
               -- contanis it, otherwise the successor is the same as the
               -- of its own first index.
               if System /= Galaxy'Last then
                  for Current in Galaxy'Succ(System)..Galaxy'Last loop
                     if Contains( Object.Data(Current), Galaxy'First ) then
                        return Current ** Galaxy'First;
                     else
                        begin
                           Return Current ** Succ( Object.Data(Current), Galaxy'First );
                        exception
                           when No_Index => Null;
                        end;
                     end if;
                  end loop;
               end if;

               Raise No_Index;
         End Succ;

      Begin
         Return
           (case Object.State is
               when Empty   =>
                  raise No_Index,
               when Full    =>
              (if Key < Universe'Last then Universe'Succ(Key)
               else raise No_Index),
               when Single  =>
              (if Key < Object.Value then Object.Value
               else raise No_Index),
               when Partial =>
              (if Key >= Max(Object) then raise No_Index
               elsif Key < Min(Object) then Min(Object)
               else Succ -- Succ(High(Key), Low(Key) )
              )
           );
      End Succ;


      Function Pred     ( Object : in     Universe_VEB;
                          Key    : in     Universe     ) return Universe is

         Function Pred( System : Galaxy:= High(Key);
                        Planet : Galaxy:= Low(Key)
                       ) return Universe is
            Use Galaxy_VEB;
         Begin
            Return Result : Universe:=
              System ** Pred( Object.Data(System), Planet );
         Exception
            when No_Index =>
               -- If we didn't find the predecessor in the the GALAXY subtree,
               -- then we need to search the previous GALAXY, recursively.
               if System /= Galaxy'First then
                  for Current in reverse Galaxy'First..Galaxy'Pred(System) loop
                     if Contains( Object.Data(Current), Galaxy'Last ) then
                        return Current ** Galaxy'Last;
                     else
                        begin
                           Return Current ** Pred( Object.Data(Current), Galaxy'Last );
                        exception
                           when No_Index => Null;
                        end;
                     end if;
                  end loop;
               end if;

               Raise No_Index;
         End Pred;

      Begin
         Return
           (case Object.State is
               when Empty   =>
                  raise No_Index,
               when Full    =>
              (if Key > Universe'First then Universe'Pred(Key)
               else raise No_Index),
               when Single  =>
              (if Key > Object.Value then Object.Value
               else raise No_Index),
               when Partial =>
              (if Key <= Min(Object) then raise No_Index
               elsif Key > Max(Object) then Max(Object)
               else Pred
              )
           );
      End Pred;


      Procedure Include ( Object : in out Universe_VEB;
                          Key    : in     Universe ) is
      Begin
         case Object.State is
            when Full   => Null;
            when Empty  => Object:= Universe_VEB'(State => Single, Value => Key);
            when Single =>
               if Object.Value /= Key then
                  declare
                     Value : Constant Universe:= Object.Value;
                  begin
                     Object:= Universe_VEB'(
                                            State   => Partial,
                                            Summary => (others => False),
                                            Data    => (others => <>),
                                            Minimum => Universe'Min(Key, Value),
                                            Maximum => Universe'Max(Key, Value)
                                           );
                     Galaxy_VEB.Include( Object.Data(High(Value)), Low(Value) );
                     Galaxy_VEB.Include( Object.Data(High(Key)), Low(Key) );
                     Object.Summary(High(Value)):= True;
                     Object.Summary(High(Key)):= True;
                  end;
               end if;
            when Partial =>
               Galaxy_VEB.Include( Object.Data(High(Key)), Low(Key) );
               Object.Summary(High(Key)):= True;
               if Is_Full( Object.Data ) then
                  Object:= Universe_VEB'(State => Full);
               else
                  Object.Minimum:= Universe'Min( Object.Minimum, Key );
                  Object.Maximum:= Universe'Max( Object.Maximum, Key );
               end if;
         end case;
      End Include;


      Procedure Exclude ( Object : in out Universe_VEB;
                          Key    : in     Universe ) is

         Generic
            Type Object is (<>);
         Package Elementary_Functions is
            Zero : Constant Object := Object'First;
            One  : Constant Object := Object'Succ( Zero );
            Two  : Constant Object := Object'Succ(  One );

            Function Succ_2( X : Object ) return Object is
              ( Object'Succ(Object'Succ( X )) );

         End Elementary_Functions;
      Begin
         case Object.State is
            when Full   =>
               Object:= Universe_VEB'(
                  State   => Partial,
                  Summary => (others => True),
                  Data    => (others => Galaxy_VEB.Create(Full => True)),
                  Minimum => (if Key = Universe'First
                              then Universe'Succ(Key)
                              else Universe'First),
                  Maximum => (if Key = Universe'Last
                              then Universe'Pred(Key)
                              else Universe'Last)
                 );
               Galaxy_VEB.Exclude(Object.Data(High(Key)), Low(Key));
            when Empty  => Null;
            when Single =>
               if Object.Value = Key then
                  Object:= Universe_VEB'(
                     State => Empty
                    );
               end if;
            when Partial =>
               -- Exclude our key.
               Galaxy_VEB.Exclude(Object.Data(High(Key)), Low(Key));
               Object.Summary(High(Key)):=
                 Not Galaxy_VEB.Is_Empty(Object.Data(High(Key)));

               if Galactic_Bitvector."="(Object.Summary, False) then
                  Object:= Universe_VEB'( State => Empty );
               else
                  declare
                     package U_Aux is new Elementary_Functions(Universe);
                     package G_Aux is new Elementary_Functions(  Galaxy);

                     Count : Universe:= U_Aux.Zero;
                     Index : Galaxy:= G_Aux.Zero;
                  begin
                     for Galaxy in Object.Data'Range loop
                        case Galaxy_VEB.State_Of( Object.Data(Galaxy) ) is
                           when Empty   => Null;
                           when Full
                              | Partial => Count:= U_Aux.Succ_2(Count);
                              Exit;
                              when Single  =>
                              Count:= Universe'Succ( Count );
                              Exit when Count > U_Aux.One;
                              Index:= Galaxy;
                        end case;
                     end loop;

                     -- If Count is exactly one, then we need the conversion os
                     -- Partial to Single.
                     if Count = U_Aux.One then
                        Object:= Universe_VEB'(
                           State => Single,
                           Value => Index ** Galaxy_VEB.Min(Object.Data(Index))
                          );
                     end if;
                  end;
               end if;
         end case;
      End Exclude;

   End VEB_Construction;





   function Convert is new
     Ada.Unchecked_Conversion( VEB_Byte, Interfaces.Unsigned_8 );
   --
   --        Function "+"(X: VEB_Byte) return Interfaces.Unsigned_8 renames Convert;


   ------------
   --  BYTE  --
   ------------


   Function Create   (Is_Full : Boolean:= False) return VEB_Byte is
     (Others => Is_Full);
   Function Is_Empty ( Object : in     VEB_Byte ) return Boolean is
     ( Convert(Object) = Interfaces.Unsigned_8'First );
   Function Is_Full  ( Object : in     VEB_Byte ) return Boolean is
     ( Convert(Object) = Interfaces.Unsigned_8'Last );
   Function State_Of (Object  : in VEB_Byte ) return Tree_State is
     (if    Is_Full (Object) then Full
      elsif Is_Empty(Object) then Empty
      elsif(for some X in Int_03 =>
                 Interfaces.Unsigned_8(2**Natural(X)) = Convert(Object) )
      then Single
      else                        Partial
     );


   Function Contains ( Object : in     VEB_Byte;
                       Key    : in     Int_03 ) return Boolean is
     (Object(Key));
   Function Minimum ( Object : in VEB_Byte; Value :Boolean:= True ) return Int_03 is
   Begin
      Return Result : Int_03:= Int_03'Last do
         SCANNER:
         For X in Int_03 loop
            if Object(X) = Value then
               Result:= X;
               Exit SCANNER;
            end if;
         end loop SCANNER;
      End Return;
   End Minimum;

   Function Maximum ( Object : in VEB_Byte; Value :Boolean:= True ) return Int_03 is
   Begin
      Return Result : Int_03:= Int_03'First do
         SCANNER:
         For X in reverse Int_03 loop
            if Object(X) = Value then
               Result:= X;
               Exit SCANNER;
            end if;
         end loop SCANNER;
      End Return;
   End Maximum;

   Function Min ( Object : in VEB_Byte ) return Int_03 is (Minimum(Object)) with SPARK_Mode => Off;
   Function Max ( Object : in VEB_Byte ) return Int_03 is (Maximum(Object)) with SPARK_Mode => Off;

   -- Returns the first AVAILABLE key; NOTE: This is not the next key.
   Function First( Object : in VEB_Byte ) return Int_03 is (Minimum(Object, False)) with SPARK_Mode => Off;
   -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
   Function Last ( Object : in VEB_Byte ) return Int_03 is (Maximum(Object, False)) with SPARK_Mode => Off;

   -- Returns the next USED key; key's value if such does not exist.
   Function Succ ( Object     : in     VEB_Byte;
                   Key        : in     Int_03 ) return Int_03 is
   Begin
      Return Result : Int_03 := Key do
         For X in Int_03'Succ(Result)..Int_03'Last loop
            if Object(X) then
               Result:= X;
               Return;
            end if;
         end loop;
      end return;
   End Succ;

   -- Returns the previous USED key; key's value if such does not exist.
   Function Pred ( Object     : in     VEB_Byte;
                   Key        : in     Int_03 ) return Int_03 is
   Begin
      Return Result : Int_03 := Key do
         For X in reverse Int_03'First..Int_03'Pred(Result) loop
            if Object(X) then
               Result:= X;
               Return;
            end if;
         end loop;
      end return;
   End Pred;

   Procedure Include ( Object : in out VEB_Byte; Key : Int_03 ) is
   Begin
      Object(Key):= True;
   End Include;

   Procedure Exclude ( Object : in out VEB_Byte; Key : Int_03 ) is
   Begin
      Object(Key):= False;
   End Exclude;

   Function Bit_Image ( Object : in VEB_Byte ) return String is
   Begin
      Return Result : String(1..Object'Length) := (others => '*') do
         For X in Object'Range loop
            Result(Result'First + Natural(X)):=
              (if Object(X) then '1' else '0');
         end loop;
      End return;
   End Bit_Image;


   --        function Convert is new
   --          Ada.Unchecked_Conversion(Bitvector_03.Bitvector, Interfaces.Unsigned_8);


   Function DEBUG_IMAGE ( Object : in VEB_Byte ) return String is
     ('(' &
      (case State_Of(Object) is
          when Full    => "F:*",
          when Empty   => "E: ",
          when Partial => "P:" & Bit_Image(Object),
          when Single  => "S:" & Interfaces.Unsigned_8'Image( Convert(Object) )
      ) & ')'
     );


   Function Convert is new Ada.Unchecked_Conversion
       (Source => EVIL.Bitvectors.Bitvector_06.Bitvector,
        Target => VEB_Byte_Collection
       );


   ------------
   --  WORD  --
   ------------

   Function Create   (Is_Full : Boolean:= False) return VEB_Word is
     ( (others => Create(Is_Full)) );

   Function State_Of ( Object : in     VEB_Word ) return Tree_State is
     (if     Is_Empty(Object) then Empty
      elsif  Is_Full (Object) then Full
      elsif (for some X in Int_03 =>
                  Interfaces.Unsigned_64(2**Natural(X)) = +Object
            )                 then Single
      else                       Partial
     );

   Function Is_Empty ( Object : in     VEB_Word ) return Boolean is
     ( Interfaces.Unsigned_64'First = +Object );
   Function Is_Full  ( Object : in     VEB_Word ) return Boolean is
     ( Interfaces.Unsigned_64'Last = +Object );
   Function Contains ( Object : in     VEB_Word;
                       Key    : in     Int_06 ) return Boolean is
     ( Contains(Object(High(Key)), Low(Key)) );

   Function Minimum  ( Object : in     VEB_Word;
                       Value  : in     Boolean:= True
                      ) return Int_06 is
   Begin
      For Galaxy_Index in Object'Range loop
         declare
            Galaxy : EVIL.Bitvectors.Bitvector_03.Bitvector renames
              EVIL.Bitvectors."+"( Convert(Object(Galaxy_Index)) );
         begin
            For System in Galaxy'Range loop
               if Galaxy(System) = Value then
                  Return Galaxy_Index ** System;
               end if;
            end loop;
         end;
      end loop;

      Return (if USE_EXCEPTIONS then raise No_Index else Int_06'Last);
   End Minimum;

   Function Maximum  ( Object : in     VEB_Word;
                       Value  : in     Boolean:= True
                      ) return Int_06 is
   Begin
      For Galaxy_Index in reverse Object'Range loop
         declare
            Galaxy : EVIL.Bitvectors.Bitvector_03.Bitvector renames
              EVIL.Bitvectors."+"( Convert(Object(Galaxy_Index)) );
         begin
            For System in reverse Galaxy'Range loop
               if Galaxy(System) = Value then
                  Return Galaxy_Index ** System;
               end if;
            end loop;
         end;
      end loop;

      Return (if USE_EXCEPTIONS then raise No_Index else Int_06'First);
   End Maximum;


   Function Min  (Object: in VEB_Word) return Int_06 is (Minimum(Object,True));
   Function Max  (Object: in VEB_Word) return Int_06 is (Maximum(Object,True));
   Function First(Object: in VEB_Word) return Int_06 is (Maximum(Object,False));
   Function Last (Object: in VEB_Word) return Int_06 is (Maximum(Object,False));

   Function Succ ( Object     : in     VEB_Word;
                   Key        : in     Int_06 ) return Int_06 is
      Galaxy : Int_03 renames High(Key);
      System : Int_03 renames Low(Key);

      This   : Constant Int_03 := Succ( Object(Galaxy), System );
   Begin
      Return Result : Int_06 := Key do
         if This /= System then
            Result:= Galaxy ** This;
         else
            For G in Int_03'Succ(Galaxy)..Int_03'Last loop
               case State_Of(Object(G)) is
                  when Empty   => null;
                  when Full    => Result:= G ** Int_03'First; Return;
                  when Partial
                     | Single  => Result:= G ** Min(Object(G)); Return;
               end case;
            end loop;
         end if;

         Pragma Warnings( Off );
         if Use_Exceptions then
            Raise No_Index;
         end if;
         Pragma Warnings( On );
      End return;
   End Succ;

   Function Pred     ( Object : in     VEB_Word;
                       Key    : in     Int_06   ) return Int_06 is
      Galaxy : Int_03 renames High(Key);
      System : Int_03 renames Low(Key);

      This   : Constant Int_03 := Pred( Object(Galaxy), System );
   Begin
      Return Result : Int_06 := Key do
         if This /= System then
            Result:= Galaxy ** This;
         else
            For G in reverse Int_03'First..Int_03'Pred(Galaxy) loop
               case State_Of(Object(G)) is
                  when Empty   => null;
                  when Full    => Result:= G ** Int_03'Last; Return;
                  when Partial
                     | Single  => Result:= G ** Max(Object(G)); Return;
               end case;
            end loop;
         end if;

         if Use_Exceptions then
            Raise No_Index;
         end if;
      End return;
   End Pred;

   Procedure Include ( Object : in out VEB_Word; Key : Int_06 ) is
   Begin
      Include(Object(High(Key)), Low(Key));
   End Include;

   Procedure Exclude ( Object : in out VEB_Word; Key : Int_06 ) is
   Begin
      Exclude(Object(High(Key)), Low(Key));
   End Exclude;

   Function Convert is new Ada.Unchecked_Conversion(
      Source => VEB_Word,
      Target => Interfaces.Unsigned_64
     );
   Function Convert is new Ada.Unchecked_Conversion(
      Source => Interfaces.Unsigned_64,
      Target => VEB_Word
     );


   Function "+"(Object : VEB_Word) return Interfaces.Unsigned_64 is (Convert(Object));
   Function "+"(Object : Interfaces.Unsigned_64) return VEB_Word is (Convert(Object));




   Function DEBUG_IMAGE ( Object : in EVIL.Bitvectors.Bitvector_06.Bitvector ) return String
     with Annotate => (Gnatprove, Terminating) is
   Begin
      Return Result : String(1..Object'Length) do
         For X in Object'Range loop
            Result(Result'First+Natural(X)):=
              (if Object(X) then '1' else '0');
         end loop;
      End return;
   End;

   Function DEBUG_IMAGE ( Object : in EVIL.Bitvectors.Bitvector_03.Bitvector ) return String is
   Begin
      Return Result : String(1..Object'Length) do
         For X in Object'Range loop
            Result(Result'First+Natural(X)):=
              (if Object(X) then '1' else '0');
         end loop;
      End return;
   End;

   Function DEBUG_IMAGE ( Object : in VEB_Word ) return String   is
      Type Byte_List is Array(Int_03 range <>) of VEB_Byte;
      Function Img( X : Byte_List ) return String is
        (case X'Length is
            when 0 => "",
            when 1 => DEBUG_IMAGE( VEB_Byte'(X(X'First)) ),
            when others =>
               Img(X(X'First..Int_03'Pred(X'Last))) &", "&
           Img(X(x'Last..x'Last))
        );
   Begin
      Return ('(' &
              (case State_Of(Object) is
                    when Full    => "F:*",
                    when Empty   => "E: ",
                    when Single  => "S:"  &
                    Interfaces.Unsigned_64'Image(Convert(Object)),
                    when Partial => "P: " & Img( Byte_List(Object) )
                )
              &')' );
   End DEBUG_IMAGE;

End EVIL.vEB;

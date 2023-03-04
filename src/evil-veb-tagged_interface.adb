With
Ada.Unchecked_Deallocation;

Package Body EVIL.vEB.Tagged_Interface with SPARK_Mode => On is
   Package Body Implementation with SPARK_Mode => On is

      Function Create_Partial(Included_1, Included_2 : Index) return Partial is
         -- Produce a subtree that is empty.
         Function Tree(Is_Full : Boolean := FALSE) return Subtree
          renames VEB_Subtree.Create;

         -- Ensure that maps contain the KEY before attempting to add the Value.
         Procedure Safe_Add(Value : Index; Object : in out Partial) with Inline is
            Use Partial_Map, VEB_Subtree;
            Hi   : Galaxy renames High(Value);
            Lo   : Galaxy renames Low (Value);
            Procedure Update(Key : Galaxy; Object : in out Subtree) is
            Begin
               Include( Object, Lo );
            End Update;

            This   : Map renames Object.Data;
            Item   : Cursor:= This.Find(Key => Hi);
            Exists : Boolean:= Partial_Map.Has_Element(Item);
         Begin
            if not Exists then
               This.Insert(New_Item => Tree, Key => Hi,
                           Position => Item, Inserted => Exists);
            end if;

            This.Update_Element(Position => Item, Process => Update'Access);
         End Safe_Add;

      Begin
         Return Result : Partial:=
           (Ada.Finalization.Controlled with Data => <>) do
            Safe_Add( Included_1, Result );
            Safe_Add( Included_2, Result );
         end return;
      End Create_Partial;

      Function Create_Partial(Excluded_1 : Index) return Partial is
         Function Tree(Is_Full : Boolean := TRUE) return Subtree
          renames VEB_Subtree.Create;
      Begin
         Return Result : Partial:=
           (Ada.Finalization.Controlled with Data => <>) do
            Declare
               Hi  : Galaxy renames High(Excluded_1);
               Lo  : Galaxy renames Low (Excluded_1);
               Procedure Update(Key : Galaxy; Object : in out Subtree) is
               Begin
                  VEB_Subtree.Exclude( Object, Lo );
               End Update;
               Map : Partial_Map.Map renames Result.Data;
               Pos : Partial_Map.Cursor;
            Begin
               -- Populate everything with a full subtree...
               For G in Galaxy loop
                  Map.Insert(New_Item  => Tree, Key => G);
               end loop;
               -- ...then exclude the single item from the appropriate subtree.
               Pos:= Map.Find( Hi );
               Map.Update_Element(Process => Update'Access, Position => Pos);
            End;
         end return;
      End Create_Partial;


      Overriding
      Procedure Finalize( Object : in out Partial ) is
      Begin
         Object.Data.Clear;
      End Finalize;

      Procedure Adjust( Object : in out VEB_Tree ) is
         Element : Controlled_Indexed'Class renames Object.Contents.Element;
      Begin
         Case Element.State_Of is
         when VEB.Full    => null;
         when VEB.Empty   => null;
         when VEB.Single  => null;
         when VEB.Partial =>
            SCANNER:
            Declare
               Index    : Galaxy;
               Count    : Natural := 0;
               All_Full : Boolean := True;
               Procedure Evaluate(X : Partial_Map.Cursor) is
                  Procedure Exclude_Key(O : in out Controlled_Indexed'Class) is
                     P : Partial renames Partial(O);
                  Begin
                     P.Data.Exclude( Partial_Map.Key(X) );
                  End;
                  Item : Subtree renames Partial_Map.Element(X);
               Begin
                  case VEB_Subtree.State_Of(Item) is
                     when VEB.Full    => Count:= Count + 2;
                     when VEB.Empty   => All_Full:= False;
                        Object.Contents.Update_Element( Exclude_Key'Access );
                     when VEB.Single  => All_Full:= False;
                        Count:= Natural'Succ(Count);
                        Index:= Partial_Map.Key(X);
                     when VEB.Partial => All_Full:= False;
                        Count:= Count + 2;
                  end case;
               End Evaluate;

               package VS renames VEB_Subtree;
               Parts    : Partial renames Partial(Element);
               This     : Partial_Map.Map renames Parts.Data;
            Begin
               This.Reverse_Iterate( Process => Evaluate'Access );
               Case Count is
                  when 0 => Make_Empty( Object );
                  when 1 => Make_Single(Object, Index ** VS.Max(This(Index)));
                  when others =>
                     if All_Full and then
                       (For all X in Galaxy => This.Contains(X)) then
                        Make_Full( Object );
                     end if;
               End case;
            End SCANNER;
         End case;
      End Adjust;


      Function Contains (Object : in     Partial;
                         Key    : in     Index) return Boolean is
        (  VEB_Subtree.Contains( Object.Data( High(Key) ), Low(Key) )  );

      Function Min      (Object : in     Partial)  return Index  is
        ( Object.Data.First_Key ** VEB_Subtree.Min(Object.Data.First_Element) );

      Function Max      (Object : in     Partial)  return Index  is
        ( Object.Data.Last_Key ** VEB_Subtree.Max(Object.Data.Last_Element) );

      Function First    (Object : in     Partial) return Index  is
      Begin
         For Index in Galaxy loop
            Begin
               Return Index **
                 (case VEB_Subtree.State_Of(Object.Data(Index)) is
                     when VEB.Full    =>  raise No_Index,
                     when VEB.Empty   =>  raise Constraint_Error, -- Should never hit.
                     when VEB.Single
                        | VEB.Partial => VEB_Subtree.First(Object.Data(Index))
                 );
            Exception
               when No_Index         => Null;
               When Constraint_Error => Return Index ** Galaxy'First;
            End;
         end loop;

         raise No_Index; -- Since this tree is PARTIAL, this should never happen.
      End First;

      Function Last     (Object : in     Partial) return Index  is
      Begin
         For Index in reverse Galaxy loop
            Begin
               Return Index **
                 (case VEB_Subtree.State_Of(Object.Data(Index)) is
                     when VEB.Full    =>  raise No_Index,
                     when VEB.Empty   =>  raise Constraint_Error, -- Should never hit.
                     when VEB.Single
                        | VEB.Partial =>  VEB_Subtree.Last(Object.Data(Index))
                 );
            Exception
               when No_Index         => Null;
               When Constraint_Error => Return Index ** Galaxy'Last;
            End;
         end loop;

         raise No_Index; -- Since this tree is PARTIAL, this should never happen.
      End Last;

      Function Succ     (Object     : in     Partial;
                         Key        : in     Index) return Index is
      Begin
         Return High(Key) ** VEB_Subtree.Succ( Object.Data(High(Key)), Low(Key) );
      Exception
         When Constraint_Error
            | No_Index =>
            For Item in Galaxy'Succ( High(Key) )..Galaxy'Last loop
               Begin
                  Return Item **
                    (case VEB_Subtree.State_Of( Object.Data(Item) ) is
                        when VEB.Empty   => Raise No_Index,
                        when VEB.Full    => Galaxy'First,
                        when VEB.Single
                           | VEB.Partial => VEB_Subtree.Min(Object.Data(Item))
                    );
               Exception
                  when Constraint_Error
                     | No_Index => Null;
               End;
            End loop;

            Raise No_Index;
      End Succ;

      -- Returns the previous USED key.
      Function Pred     (Object     : in     Partial;
                         Key        : in     Index) return Index is
      Begin
         Return High(Key) ** VEB_Subtree.Pred(Object.Data(High(Key)), Low(Key));
      Exception
         When Constraint_Error
            | No_Index =>
            For Item in reverse Galaxy'First..Galaxy'Pred( High(Key) ) loop
               Declare
               Begin
                  Return Item **
                    (case VEB_Subtree.State_Of( Object.Data(Item) ) is
                        when VEB.Empty   => Raise No_Index,
                        when VEB.Full    => Galaxy'Last,
                        when VEB.Single
                           | VEB.Partial  => VEB_Subtree.Max(Object.Data(Item))
                    );
               Exception
                  when Constraint_Error
                     | No_Index => Null;
               End;
            End loop;

            Raise No_Index;
      End Pred;

      Procedure Include (Object     : in out Partial; Key : Index) is
      Begin
         if not Object.Data.Contains( High(Key) ) then
            Object.Data.Insert(New_Item  => VEB_Subtree.Create(Full => False),
                               Key       => High(Key)
                              );
         end if;
         VEB_Subtree.Include( Object.Data(High(Key)), Low(Key) );
      End Include;

      Procedure Exclude (Object     : in out Partial; Key : Index) is
      Begin
         VEB_Subtree.Exclude( Object.Data(High(Key)), Low(Key) );
      End Exclude;


      --------------------------------------------------------------------------------
      --------------------------------------------------------------------------------
      --------------------------------------------------------------------------------

      Function State_Of (Object : in     VEB_Tree) return Tree_State is
        (  Object.Contents.Element.State_Of  );

      Function Is_Empty (Object : in     VEB_Tree)  return Boolean is
        (  Object.Contents.Is_Empty or else Object.Contents.Element.Is_Empty  );

      Function Is_Full  (Object : in     VEB_Tree)  return Boolean is
        (  Object.Contents.Element.Is_Full  );

      Function Min      (Object : in     VEB_Tree)  return Index   is
        (  Object.Contents.Element.Min  );

      Function Max      (Object : in     VEB_Tree)  return Index   is
        (  Object.Contents.Element.Max  );

      Function First    (Object : in     VEB_Tree)  return Index   is
        (  Object.Contents.Element.First  );

      Function Last     (Object : in     VEB_Tree)  return Index   is
        (  Object.Contents.Element.Last  );

      -- Returns the next USED key.
      Function Succ     (Object     : in     VEB_Tree;
                         Key        : in     Index) return Index   is
        (  Object.Contents.Element.Succ(Key)  );

      Function Pred     (Object     : in     VEB_Tree;
                         Key        : in     Index)  return Index  is
        (  Object.Contents.Element.Pred(Key)  );

      Procedure Include (Object     : in out VEB_Tree; Key: Index) is
         This : Holder.Holder renames Object.Contents;
         Item : Controlled_Indexed'Class renames This.Element;
      Begin
         Case Item.State_Of is
         when VEB.Full    => null;
         when VEB.Empty   => Make_Single( Object, Key );
         when VEB.Partial =>
            Declare
               H_K   : Constant Galaxy:= High( Key );
               Value : Constant Galaxy:= Low ( Key );
               Procedure Update(Key : Galaxy; Element : in out Subtree) is
               Begin
                  VEB_Subtree.Include( Element, Value );
               End Update;
               Procedure Update(Element : in out Controlled_Indexed'Class) is
                  Map  : Partial_Map.Map renames Partial(Element).Data;
                  X    : Partial_Map.Cursor := Map.Find( H_K );
                  I    : Boolean:= False;
               Begin
                  if Not Partial_Map.Has_Element(X) then
                     Map.Insert(Position => X, Inserted => I, Key => H_K,
                                New_Item => VEB_Subtree.Create(Full => FALSE)
                               );
                  end if;

                  Map.Update_Element(Process => Update'Access, Position => X);
               End Update;
            Begin
               This.Update_Element( Update'Access );
            End;
            Object.Adjust;
         when VEB.Single  =>
            Declare
               Value : Index renames Single(Item).Value;
            Begin
               if Value /= Key then
                  Make_Partial( Object, V1 => Value, V2 => Key );
               end if;
            End;
         end case;
      End Include;

      Procedure Exclude (Object     : in out VEB_Tree; Key: Index)  is
         This : Holder.Holder renames Object.Contents;
         Item : Controlled_Indexed'Class renames This.Element;
      Begin
         Case Item.State_Of is
         when VEB.Full    => Make_Partial(Object, E1 => Key);
         when VEB.Empty   => null;
         when VEB.Partial =>
            Declare
               H_K : Constant Galaxy:= High( Key );
               L_K : Constant Galaxy:= Low ( Key );
               Procedure Update(Key : Galaxy; Element : in out Subtree) is
               Begin
                  VEB_Subtree.Exclude( Element, L_K );
               End Update;
               Procedure Update(Element : in out Controlled_Indexed'Class) is
                  Map : Partial_Map.Map renames Partial(Element).Data;
                  X   : Partial_Map.Cursor:= Map.Find( Key => H_K );
               Begin
                  Map.Update_Element(Position => X, Process => Update'Access);
               End Update;

            Begin
               This.Update_Element( Update'Access );
            End;
            Object.Adjust;
         when VEB.Single  =>
            if Key = Single(Item).Value then
               Make_Empty( Object );
            end if;
         end case;
      End Exclude;

      Procedure Make_Empty  (Object : in out VEB_Tree) is
      Begin
         Object.Contents.Replace_Element(
            New_Item => Empty'(Ada.Finalization.Controlled with others => <>)
           );
      End Make_Empty;

      Procedure Make_Full   (Object : in out VEB_Tree) is
      Begin
         Object.Contents.Replace_Element(
            New_Item => Full'(Ada.Finalization.Controlled with others => <>)
           );
      End Make_Full;

      Procedure Make_Single (Object : in out VEB_Tree; Value : Index) is
      Begin
         Object.Contents.Replace_Element(
            New_Item => Single'(Ada.Finalization.Controlled with Value => Value)
           );
      End Make_Single;

      Procedure Make_Partial(Object : in out VEB_Tree; V1, V2: Index) is
      Begin
         Object.Contents.Replace_Element(
            New_Item => Create_Partial(Included_1 => V1, Included_2 =>  V2)
           );
      End Make_Partial;

      Procedure Make_Partial(Object : in out VEB_Tree; E1    : Index) is
      Begin
         Object.Contents.Replace_Element(
            New_Item => Create_Partial(Excluded_1 => E1)
           );
      End Make_Partial;


      -----------------
      --  DEBUGGING  --
      -----------------

      Function DEBUG_IMAGE(Object : in     VEB_Tree) return String
      with SPARK_Mode => Off is
         Package String_Vector is new Ada.Containers.Indefinite_Ordered_Maps(
            Key_Type     => Galaxy,
            Element_Type => String
           );

         Function DEBUG_IMAGE(Item : in Partial) return String is
            Result : String_Vector.Map:= String_Vector.Empty_Map;
            Length : Natural:= 0;
            Procedure Append(X : Partial_Map.Cursor) is
               Key   : Galaxy  renames Partial_Map.Key(X);
               Value : Subtree renames Partial_Map.Element(X);
               As_String : Constant String:=
                 '('& VEB_Subtree.DEBUG_IMAGE(Value) &')';
            Begin
               Length:= Length + As_String'Length;
               Result.Include(Key => Key, New_Item => As_String);
--                 Result.Replace_Element(
--                    Result.Element &
--                    Galaxy'Image(Key) & ':' & VEB_Subtree.DEBUG_IMAGE(Value)
--                   );
            End Append;
            Current : Positive:= 1;

            Generic
               Object : in out Partial_Map.Map;
            Function Collect(Working : String:= "") return String;
            Function Collect(Working : String:= "") return String is
               Last : Partial_Map.Cursor renames Partial_Map.Last( Object );
            begin
               if not Partial_Map.Has_Element(Last) then
                  return Working & ".";
               else
                  Declare
                     Use Partial_Map, VEB_Subtree;
                     Index : String renames Galaxy'Image( Key(Last) );
                     Value : String renames DEBUG_IMAGE(Element(Last));
                  begin
                     Object.Delete_Last;
                     return Collect(Working & Index & "->" & Value);
                  end;
               end if;
            end Collect;
            Copy : Partial_Map.Map:= Partial_Map.Copy(Item.Data);
            Function Collector is new Collect( Copy );
         Begin
            Item.Data.Iterate( Append'Access );
            Return Working : Constant String:= Collector;
--                (Current..Length):= (others => '$') do
--                 For Value of Result loop
--                    Declare
--                       Stop  : Constant Natural:= Natural'Pred( Current+Value'Length );
--                       Chunk : String renames Working(Current..Stop);
--                    Begin
--                       Chunk:= Value;
--                       Current:= Natural'Succ(Stop);
--                    End;
--                 end loop;
--              End return;
              --String_Holder.Element( Result ) & '.';
         End DEBUG_IMAGE;

         Item : Controlled_Indexed'Class renames Object.Contents.Element;
      Begin
         Return '(' &
         (case Object.State_Of is
             when VEB.Full    => "F:*",
             when VEB.Empty   => "E: ",
             when VEB.Single  => "S:" & Index'Image(Single(Item).Value),
             when VEB.Partial => "P:" & DEBUG_IMAGE(Partial(Item))
         ) & ')';
      End DEBUG_IMAGE;
   End Implementation;
End EVIL.vEB.Tagged_Interface;

With
Ada.Finalization,
Ada.Containers.Indefinite_Holders,
Ada.Containers.Indefinite_Ordered_Maps;

-- EVIL.vEB.Tagged_Interface
--	This package takes an index and defines further defines an interface for
--	the VAN EMDE BOAS tree, called INDEXED, thus any type of Indexed'Class
--	fulfils the interface of the ADT.
--
-- Contents:
--	* INDEXED:		The interface dealing with the operations which
--				concern the generic formal parameter INDEX.
--	* IMPLEMENTATION:	A generic package taking an instantiation of the
--				VEB_INTERFACE package, its index-type, a new
--				index-type (of twice the bits), and attendant
--				conversion-functions to produce a VEB-subtree.
Generic
   Type Index is (<>);
Package EVIL.vEB.Tagged_Interface with Preelaborate, Remote_Types, SPARK_Mode => On is

   Type Indexed is interface and Base
   with Preelaborable_Initialization;

   Function Min      (Object : in     Indexed)   return Index   is abstract;
   Function Max      (Object : in     Indexed)   return Index   is abstract;

   -- Returns the first AVAILABLE key; NOTE: This is not the next key.
   Function First    (Object : in     Indexed)   return Index   is abstract;
   -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
   Function Last     (Object : in     Indexed)   return Index   is abstract;

   -- Returns the next USED key.
   Function Succ     (Object     : in     Indexed;
                      Key        : in     Index) return Index   is abstract;
   -- Returns the previous USED key.
   Function Pred     (Object     : in     Indexed;
                      Key        : in     Index) return Index   is abstract;
   Procedure Include (Object     : in out Indexed; Key: Index)  is abstract;
   Procedure Exclude (Object     : in out Indexed; Key: Index)  is abstract;

   -- EVIL.vEB.Tagged_Interface.Implementation
   --	This package takes the SUBTREE, its and instantiation of VEB_INTERFACE
   --	with said subtree, and operations for constructing and deconstructing
   --	Universe (INDEX) values into GALAXY-sized values. These GALAXY-sized
   --	values are then used to map into a SUBTREE.
   --
   --	NOTE:	The publically visible type VEB_TREE is merely a holder of the
   --		private CONTROLLED_INDEXED class, which internally implements
   --		functions for changing between the FULL, PARTIAL, SINGLE & EMPTY
   --		states as well as deallocation of the pointer-to-map structure
   --		used for the PARTIAL class/type.
   Generic
      Type Galaxy      is (<>);
      Type Subtree(<>) is private;
      with Function High(Input : Index) return Galaxy is <>;
      with Function Low (Input : Index) return Galaxy is <>;
      with Function "**"(Left, Right : Galaxy)  return Index  is <>;
      --with Function "=" (Left, Right : Subtree) return Boolean is <>;
      with Package VEB_Subtree is new EVIL.VEB.VEB_Interface
        (Int_X => Galaxy, VEB_X => Subtree, others => <>);
   Package Implementation with SPARK_Mode => On is


      --Function "="(Left, Right : in     Indexed'Class) return Boolean;
      --     Procedure Include(Item : in out Holder.Holder; Value : Index) is null;
      --     Procedure Exclude(Item : in out Holder.Holder; Value : Index) is null;

      Type VEB_Tree is new Ada.Finalization.Controlled and Indexed
      with private;

      Overriding
      Procedure Adjust  (Object : in out VEB_Tree);
      Function State_Of (Object : in     VEB_Tree) return Tree_State;
      Function Is_Empty (Object : in     VEB_Tree) return Boolean;
      Function Is_Full  (Object : in     VEB_Tree) return Boolean;
      Function Min      (Object : in     VEB_Tree) return Index;
      Function Max      (Object : in     VEB_Tree) return Index;

      -- Returns the first AVAILABLE key; NOTE: This is not the next key.
      Function First    (Object : in     VEB_Tree)   return Index;
      -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
      Function Last     (Object : in     VEB_Tree)   return Index;

      -- Returns the next USED key.
      Function Succ     (Object     : in     VEB_Tree;
                         Key        : in     Index) return Index;
      -- Returns the previous USED key.
      Function Pred     (Object     : in     VEB_Tree;
                         Key        : in     Index)  return Index;
      Procedure Include (Object     : in out VEB_Tree; Key: Index);
      Procedure Exclude (Object     : in out VEB_Tree; Key: Index);

      Function DEBUG_IMAGE (Object : in     VEB_Tree) return String;
   Private
      Type Controlled_Indexed is abstract new Ada.Finalization.Controlled
           and Indexed
      with null record;

      Package Internals with SPARK_Mode => Off is
         Package Partial_Map is new Ada.Containers.Indefinite_Ordered_Maps(
            --           "<"          => ,
            --           "="          => ,
            Key_Type     => Galaxy,
            Element_Type => Subtree
           );
      End Internals;
      Package Partial_Map renames Internals.Partial_Map;

      Type Empty   is new Controlled_Indexed with null Record;
      Type Full    is new Controlled_Indexed with null Record;
      Type Single  is new Controlled_Indexed with record
         Value : Index;
      end record;
      Type Partial is new Controlled_Indexed with Record
         Data : Partial_Map.Map:= Partial_Map.Empty_Map;
      End record;

      -- Given two values, create an object that contains those two items.
      Function Create_Partial(Included_1, Included_2 : Index) return Partial;

      -- Given a single value, create an object that contains all other values.
      Function Create_Partial(Excluded_1             : Index) return Partial;


      Package Holder is new Ada.Containers.Indefinite_Holders
        (Element_Type => Controlled_Indexed'Class);

      Function Default return Controlled_Indexed'Class is
        (  Empty'(Ada.Finalization.Controlled with others => <>)  );

      Procedure Make_Empty  (Object : in out VEB_Tree) with Inline;
      Procedure Make_Full   (Object : in out VEB_Tree) with Inline;
      Procedure Make_Single (Object : in out VEB_Tree; Value : Index) with Inline;
      Procedure Make_Partial(Object : in out VEB_Tree; V1, V2: Index) with Inline;
      Procedure Make_Partial(Object : in out VEB_Tree; E1    : Index) with Inline;

      Type VEB_Tree is new Ada.Finalization.Controlled and Indexed with record
         Contents : Holder.Holder:= Holder.To_Holder(  Default  );
      end record;
--      with Type_Invariant => not Holder.Is_Empty( VEB_Tree.Contents );



      -------------
      --  EMPTY  --
      -------------

      Function Is_Empty (Object : in     Empty)  return Boolean is (True);
      Function Is_Full  (Object : in     Empty)  return Boolean is (False);
      Function Contains (Object : in     Empty;
                         Key    : in     Index)  return Boolean is (False);
      Function Min      (Object : in     Empty)  return Index   is (raise No_Index);
      Function Max      (Object : in     Empty)  return Index   is (raise No_Index);

      -- Returns the first AVAILABLE key; NOTE: This is not the next key.
      Function First    (Object : in     Empty) return Index   is (Index'First);
      -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
      Function Last     (Object : in     Empty) return Index  is (Index'Last);

      -- Returns the next USED key.
      Function Succ     (Object     : in     Empty;
                         Key        : in     Index) return Index is (Key);
      -- Returns the previous USED key.
      Function Pred     (Object     : in     Empty;
                         Key        : in     Index) return Index is (Key);
      Procedure Include (Object     : in out Empty; Key:Index) is null; --(raise Program_Error);
      Procedure Exclude (Object     : in out Empty; Key:Index) is null; --(raise Program_Error);
      function State_Of( Object : in Empty ) return Tree_State  is
        (VEB.Empty);

      ------------
      --  FULL  --
      ------------

      Function Is_Empty (Object : in     Full)  return Boolean is (False);
      Function Is_Full  (Object : in     Full)  return Boolean is (True);
      Function Contains (Object : in     Full;
                         Key    : in     Index) return Boolean is (True);
      Function Min      (Object : in     Full)  return Index  is (Index'First);
      Function Max      (Object : in     Full)  return Index  is (Index'Last);

      -- Returns the first AVAILABLE key; NOTE: This is not the next key.
      Function First    (Object : in     Full) return Index  is (raise No_Index);
      -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
      Function Last     (Object : in     Full) return Index  is (raise No_Index);

      -- Returns the next USED key.
      Function Succ     (Object     : in     Full;
                         Key        : in     Index) return Index is
        (if Key /= Index'Last then Index'Succ(Key) else raise No_Index);
      -- Returns the previous USED key.
      Function Pred     (Object     : in     Full;
                         Key        : in     Index) return Index is
        (if Key /= Index'First then Index'Pred(Key) else raise No_Index);
      Procedure Include (Object     : in out Full; Key:Index) is null; --(raise Program_Error);
      Procedure Exclude (Object     : in out Full; Key:Index) is null; --(raise Program_Error);

      function State_Of( Object : in Full) return Tree_State  is
        (VEB.Full);

      --------------
      --  SINGLE  --
      --------------

      Function Is_Empty (Object : in     Single)  return Boolean is (False);
      Function Is_Full  (Object : in     Single)  return Boolean is (False);
      Function Contains (Object : in     Single;
                         Key    : in     Index) return Boolean is
        (Key = Object.Value);
      Function Min      (Object : in     Single)  return Index  is (Object.Value);
      Function Max      (Object : in     Single)  return Index  is (Object.Value);

      -- Returns the first AVAILABLE key; NOTE: This is not the next key.
      Function First    (Object : in     Single) return Index  is
        (if Object.Value /= Index'First then Index'First else Index'Succ(Index'First));
      -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
      Function Last     (Object : in     Single) return Index  is
        (if Object.Value /= Index'Last then Index'Last else Index'Pred(Index'Last));

      -- Returns the next USED key.
      Function Succ     (Object     : in     Single;
                         Key        : in     Index) return Index is
        (if Key < Object.Value then Object.Value else raise No_Index);
      -- Returns the previous USED key.
      Function Pred     (Object     : in     Single;
                         Key        : in     Index) return Index is
        (if Key > Object.Value then Object.Value else raise No_Index);
      Procedure Include (Object     : in out Single; Key: Index) is null; --(raise Program_Error);
      Procedure Exclude (Object     : in out Single; Key: Index) is null; --(raise Program_Error);

      function State_Of( Object : in Single ) return Tree_State  is
        (VEB.Single);


      --------------
      -- PARTIAL  --
      --------------


      Overriding Procedure Finalize( Object : in out Partial );


      Function Is_Empty (Object : in     Partial)  return Boolean is (False);
      Function Is_Full  (Object : in     Partial)  return Boolean is (False);
      Function Contains (Object : in     Partial;
                         Key    : in     Index) return Boolean;
      Function Min      (Object : in     Partial)  return Index;
      Function Max      (Object : in     Partial)  return Index;

      -- Returns the first AVAILABLE key; NOTE: This is not the next key.
      Function First    (Object : in     Partial) return Index;
      -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
      Function Last     (Object : in     Partial) return Index;

      -- Returns the next USED key.
      Function Succ     (Object     : in     Partial;
                         Key        : in     Index) return Index;
      -- Returns the previous USED key.
      Function Pred     (Object     : in     Partial;
                         Key        : in     Index) return Index;
      Procedure Include (Object     : in out Partial; Key:Index);
      Procedure Exclude (Object     : in out Partial; Key:Index);

      function State_Of( Object : in Partial ) return Tree_State  is
        (VEB.Partial);



      -----------------------
      --  CLASS FUNCTIONS  --
      -----------------------
      Function Create(Is_Full : Boolean:= False) return Indexed'Class is
        (if Is_Full then Full' (Ada.Finalization.Controlled with others => <>)
         else            Empty'(Ada.Finalization.Controlled with others => <>)
        );
      --          (if Is_Full then  Full'(Ada.Finalization.Controlled with others => <>)
      --           else            Empty'(Ada.Finalization.Controlled with others => <>)
      --          );

   End Implementation;

End EVIL.vEB.Tagged_Interface;

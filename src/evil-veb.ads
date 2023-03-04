Pragma Ada_2012;
Pragma Assertion_Policy( Check );

with
EVIL.Generic_Bitvector;

private with
EVIL.Bitvectors;

-- EVIL.vEB
--	This package defines the VAN EMDE BOAS tree, a data-structure for fast
--	successor/predecessor function, as well as simple SET functions (namely
--	include/exclude).
--
-- Contents:
--	* VEB_INTERFACE:	A generic package allowing the given data-type
--				and functions to be used to implement/interface
--				as a VEB-tree.
--	* VEB_CONSTRUCTION:	A generic package taking an instantiation of the
--				VEB_INTERFACE package, its index-type, a new
--				index-type (of twice the bits), and attendant
--				conversion-functions to produce a VEB-tree.
--
--	* BASE:			Interface for tagged-type derrivation.
--	* VEB_BYTE:		A VEB using a single byte.     ( 8-element set.)
--	* VEB_WORD:		A VEB using a 64-bit integer.  (64-element set.)
Package EVIL.vEB with Pure, SPARK_Mode => On, Elaborate_Body is

   -- Enumeration of the various states of a VEB tree; this is used in the
   -- tagged-type implementations of the trees for space-optimization.
   -- States:
   --	Empty:   a [sub]tree with no elements set.
   --	Single:  a [sub]tree containing a single element.
   --	Partial: a [sub]tree with some, but not all, elements set.
   --	Full:    a [sub]tree with all elements set.
   Type Tree_State is (Empty, Single, Partial, Full)
     with Size => 2;

   -- This is the base interface of the tagged-type implementation.
   Type Base is interface;
   Function State_Of (Object  : in     Base) return Tree_State is abstract;
   Function Is_Empty (Object  : in     Base) return Boolean    is abstract;
   Function Is_Full  (Object  : in     Base) return Boolean    is abstract;
--     Function Create   (Object  : in     Base;
--                        Is_Full : in     Boolean:= False
--                       )                       return Boolean    is abstract;

   ---

   -- This generic defines the core interface of the VEB data-structure, it is
   -- NOT a tagget type for two reasons (1) to allow for the usage of bitvectors
   -- [for "small" types w/o the overhead of the tag] and other non-tagged types
   -- to implement the interface; and (2) to allow for the [semi-]recursive
   -- construction in terms of the interface.
   Generic
      -- The Index type.
      Type Int_X     is (<>);

      -- Any non-limited type with which to implement/represent a VEB [sub]tree.
      Type VEB_X(<>) is private;

      -- Creates a VEB subtree, either empty or with all elements set.
      with Function Create   (Full    : Boolean:= False) return VEB_X    is <>;

      -- Returns the state of the tree; this is useful in optimizing operations.
      -- EXAMPLE: for an EMPTY tree, we know that the first available element is
      -- the first item of INT_X, whereas with a SINGLE we can query the MIN and
      -- if the value is the first return the second, otherwise it is the first.
      with Function State_Of ( Object : in     VEB_X ) return Tree_State is <>;

      -- Functions querying the general state of the tree; these are  included
      -- for the convienience of having a BOOLEAN return vaule, as well as for
      -- allowing the optimization of the generic-construction's implementation
      -- of the STATE_OF function when applied to a subtree; example:
      --	Is_Empty(O: VEB_X) -> (for all X of O.SUBTREE => Is_Empty(X))
      with Function Is_Empty ( Object : in     VEB_X ) return Boolean    is <>;
      with Function Is_Full  ( Object : in     VEB_X ) return Boolean    is <>;

      -- For a non-leaf [sub]tree, the CONTAINS function is recursive on the
      -- size of the index, reducing the index bit-size by half each time; this
      -- is how fast lookup times are achieved: partitioning the structure being
      -- indexed into SQRT(INDEX_SIZE) groups of SQRT(INDEX_SIZE) elements.
      with Function Contains ( Object : in     VEB_X;
                               Key    : in     Int_X ) return Boolean    is <>;

      -- Return the extremes of the tree; this is useful for optimizing the
      -- SUCC and PRED functions; example:
      --	if X >= MAX(O) then there is no sucessor-element in the tree.
      with Function Min      ( Object : in     VEB_X ) return Int_X      is <>;
      with Function Max      ( Object : in     VEB_X ) return Int_X      is <>;

      -- Returns the first AVAILABLE key; NOTE: This is not the next key.
      with Function First    ( Object : in     VEB_X ) return Int_X      is <>;
      -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
      with Function Last     ( Object : in     VEB_X ) return Int_X      is <>;

      -- Returns the next USED key.
      with Function Succ ( Object     : in     VEB_X;
                           Key        : in     Int_X ) return Int_X      is <>;
      -- Returns the previous USED key.
      with Function Pred ( Object     : in     VEB_X;
                           Key        : in     Int_X ) return Int_X      is <>;

      -- Operations to set or remove an element within the tree.
      with Procedure Include ( Object : in out VEB_X; Key : Int_X )      is <>;
      with Procedure Exclude ( Object : in out VEB_X; Key : Int_X )      is <>;

      ---------------
      -- DEBUGGING --
      ---------------
      with Function DEBUG_IMAGE ( Object : in VEB_X ) return String      is <>;

   Package VEB_Interface is
   End VEB_Interface;

   -- Given X / bitvector implementation
   Generic
      Type Universe is (<>);
      Type Galaxy   is (<>);
      Type Subtree  is private;

      with Package Galaxy_VEB is new VEB_Interface(
         Int_X  => Galaxy,
         VEB_X  => Subtree,
         others => <>
        );
      with Package Galactic_Bitvector is new EVIL.Generic_Bitvector(
         Elements => Galaxy
        );

      -- Index construction/deconstruction functions.
      with Function High(X : Universe) return Galaxy is <>;
      with Function Low (X : Universe) return Galaxy is <>;
      with Function "**"(Left, Right : Galaxy) return Universe is <>;
   Package VEB_Construction is
      Type Galactic_Cluster is Array(Galaxy) of Subtree;
      Function Is_Full(Object : Galactic_Cluster) return Boolean;
      Function DEBUG_IMAGE ( Object : in Galactic_Cluster ) return String;

      type Universe_VEB(State : Tree_State:= Empty) is record
         case State is
            when Full
               | Empty   => Null;
            when Single  => Value : Universe;
            when Partial =>
               Minimum,
               Maximum : Universe;
               Summary : Galactic_Bitvector.Bitvector;
               Data    : Galactic_Cluster;
         end case;
      end record
      with Pack;

      Function Create   ( Is_Full: Boolean:= False) return Universe_VEB;
      Function State_Of ( Object : in     Universe_VEB ) return Tree_State;

      Function Is_Empty ( Object : in     Universe_VEB ) return Boolean;
      Function Is_Full  ( Object : in     Universe_VEB ) return Boolean;
      Function Contains ( Object : in     Universe_VEB;
                          Key    : in     Universe     ) return Boolean;
      Function Min      ( Object : in     Universe_VEB ) return Universe
        with Pre => Not Is_Empty(Object) or else raise No_Index;
      Function Max      ( Object : in     Universe_VEB ) return Universe
        with Pre => Not Is_Empty(Object) or else raise No_Index;

      -- Returns the first AVAILABLE key; NOTE: This is not the next key.
      Function First    ( Object : in     Universe_VEB ) return Universe
        with Pre => Not Is_Full(Object);
      -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
      Function Last     ( Object : in     Universe_VEB ) return Universe
        with Pre => Not Is_Full(Object);

      -- Returns the next USED key.
      Function Succ     ( Object : in     Universe_VEB;
                          Key    : in     Universe     ) return Universe;
      -- Returns the previous USED key.
      Function Pred     ( Object : in     Universe_VEB;
                          Key    : in     Universe     ) return Universe;

      Procedure Include ( Object : in out Universe_VEB;
                          Key    : in     Universe     )
        with Post => Not Is_Empty(Object) and Contains(Object, Key);
      Procedure Exclude ( Object : in out Universe_VEB;
                          Key    : in     Universe     )
        with Post => Not Is_Full(Object) and Not Contains(Object, Key);

      Function DEBUG_IMAGE ( Object : in Universe_VEB ) return String;

      Package This_Interface is new VEB_Interface( Universe, Universe_VEB );
   Private
      Function Is_Full(Object : Galactic_Cluster) return Boolean is
        (for all X of Object => Galaxy_VEB.Is_Full(X));
   End VEB_Construction;


   --  -----------------------------------------------------------------
   --  -----------------------------------------------------------------
   --  -----------------------------------------------------------------
   --  -----------------------------------------------------------------
   Type VEB_Word is private;

   Function Create   ( Is_Full: in Boolean:= False ) return VEB_Word;
   Function State_Of ( Object : in VEB_Word ) return Tree_State
     with Annotate =>(Gnatprove, Terminating);
   Function Is_Empty ( Object : in VEB_Word ) return Boolean;
   Function Is_Full  ( Object : in VEB_Word ) return Boolean;
   Function Contains ( Object : in VEB_Word; Key : Int_06 ) return Boolean;

   Function Min( Object : in VEB_Word ) return Int_06
     with Pre => not Is_Empty(Object);

   Function Max( Object : in VEB_Word ) return Int_06
     with Pre => not Is_Empty(Object);


   -- Returns the first AVAILABLE key; NOTE: This is not the next key.
   Function First( Object : in VEB_Word ) return Int_06
     with Pre => Not Is_Full(Object);
   -- Returns the last AVAILABLE key; NOTE: This is not the previous key.
   Function Last( Object : in VEB_Word ) return Int_06
     with Pre => Not Is_Full(Object);

   -- Returns the next USED key.
   Function Succ ( Object : in VEB_Word; Key : Int_06 ) return Int_06;
   -- Returns the previous USED key.
   Function Pred ( Object : in VEB_Word; Key : Int_06 ) return Int_06;


   Procedure Include ( Object : in out VEB_Word; Key : Int_06 )
     with Post => Not Is_Empty(Object);
   Procedure Exclude ( Object : in out VEB_Word; Key : Int_06 )
     with Post => Not Is_Full(Object);

   Function DEBUG_IMAGE ( Object : in VEB_Word ) return String;
   Function "+"(Object : VEB_Word) return Interfaces.Unsigned_64;
   Function "+"(Object : Interfaces.Unsigned_64) return VEB_Word;


   ------------
   --  BYTE  --
   ------------
   Type VEB_Byte is private;

   Function Create   (Is_Full : Boolean:= False ) return VEB_Byte;
   Function Is_Empty ( Object : in     VEB_Byte ) return Boolean;
   Function Is_Full  ( Object : in     VEB_Byte ) return Boolean;
   Function State_Of ( Object : in     VEB_Byte ) return Tree_State
     with Annotate => (Gnatprove, Terminating);
   Function Contains ( Object : in     VEB_Byte;
                       Key    : in     Int_03   ) return Boolean;
   Function Minimum  ( Object : in     VEB_Byte;
                       Value  : in     Boolean:= True
                     ) return Int_03;
   Function Maximum  ( Object : in     VEB_Byte;
                       Value  : in     Boolean:= True
                     ) return Int_03;
   Function Min      ( Object : in     VEB_Byte ) return Int_03;
   Function Max      ( Object : in     VEB_Byte ) return Int_03;
   Function First    ( Object : in     VEB_Byte ) return Int_03;
   Function Last     ( Object : in     VEB_Byte ) return Int_03;

   Function Succ     ( Object : in     VEB_Byte;
                       Key    : in     Int_03   ) return Int_03;
   Function Pred     ( Object : in     VEB_Byte;
                       Key    : in     Int_03   ) return Int_03;

   Procedure Include ( Object : in out VEB_Byte; Key : Int_03 );
   Procedure Exclude ( Object : in out VEB_Byte; Key : Int_03 );

   Function Bit_Image( Object : in VEB_Byte ) return String
     with Annotate => (Gnatprove, Terminating);

   Function DEBUG_IMAGE ( Object : in VEB_Byte ) return String;


--     Package Byte_Interface is new VEB_Interface( Int_03, VEB_Byte );
--     Package Word_Interface is new VEB_Interface( Int_06, VEB_Word );
Private

   -- Here we use a byte-sized bitvector as a VEB, we have to use
   -- type-derrivation in order to allow the publicly visible contract.
   Type VEB_Byte is new Bitvectors.Bitvector_03.Bitvector;
   Use all type VEB_Byte;


   Type TS_Array_03 is Array (Int_03) of Tree_State
     with Component_Size => 2, Size => 16;
   Type VEB_Byte_Collection is Array(Int_03) of VEB_Byte
     with Component_Size => 8, Size => 64;

   ------------
   --  WORD  --
   ------------
   Type VEB_Word is array (Int_03) of VEB_Byte
     with Component_Size => 8, Size => 64;


   Type Tree( Size : Unsigned_32 ) is tagged null record;

End EVIL.vEB;

with
Ada.Streams;

Package Testbed.Pure_IO with Pure is
   Subtype String is Standard.String;
   Prefix : Constant String := "PIO_";

   type File_Type is limited private;
   type File_Mode is (In_File, Out_File, Append_File);
   for File_Mode use
     (In_File     => 0,
      Out_File    => 2,
      Append_File => 3
     );

   type Count is range 0 .. Natural'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;


--------------------------------------------------------------------------------
   -- with Import, Link_Name => Prefix & "";
   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := ""
     ) with Import, Link_Name => Prefix & "Create1";

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := ""
     ) with Import, Link_Name => Prefix & "Open1";

   procedure Close  (File : in out File_Type)
    with Import, Link_Name => Prefix & "Close1";
   procedure Delete (File : in out File_Type)
    with Import, Link_Name => Prefix & "Delete1";
   procedure Reset  (File : in out File_Type; Mode : File_Mode)
    with Import, Link_Name => Prefix & "Reset1";
   procedure Reset  (File : in out File_Type)
    with Import, Link_Name => Prefix & "Reset2";

   function Mode (File : File_Type) return File_Mode
    with Import, Link_Name => Prefix & "Mode1";
   function Name (File : File_Type) return String
    with Import, Link_Name => Prefix & "Name1";
   function Form (File : File_Type) return String
    with Import, Link_Name => Prefix & "Form1";

   function Is_Open (File : File_Type) return Boolean
    with Import, Link_Name => Prefix & "Is_Open1";

   ------------------------------------------------------
   -- Control of default input, output and error files --
   ------------------------------------------------------

   procedure Set_Input  (File : File_Type)
    with Import, Link_Name => Prefix & "Set_Input1";
   procedure Set_Output (File : File_Type)
    with Import, Link_Name => Prefix & "Set_Output1";
   procedure Set_Error  (File : File_Type)
    with Import, Link_Name => Prefix & "Set_Error1";

   function Standard_Input  return File_Type
    with Import, Link_Name => Prefix & "Standard_Input1";
   function Standard_Output return File_Type
    with Import, Link_Name => Prefix & "Standard_Output1";
   function Standard_Error  return File_Type
    with Import, Link_Name => Prefix & "Etandard_Error1";


   function Current_Input  return File_Type
    with Import, Link_Name => Prefix & "";
   function Current_Output return File_Type
    with Import, Link_Name => Prefix & "";
   function Current_Error  return File_Type
    with Import, Link_Name => Prefix & "";

--     type File_Access is access constant File_Type;
--
--     function Standard_Input  return File_Access;
--     function Standard_Output return File_Access;
--     function Standard_Error  return File_Access;
--
--     function Current_Input  return File_Access;
--     function Current_Output return File_Access;
--     function Current_Error  return File_Access;

   --------------------
   -- Buffer control --
   --------------------

   --  Note: The parameter file is IN OUT in the RM, but this is clearly
   --  an oversight, and was intended to be IN, see AI95-00057.

   procedure Flush (File : File_Type)
    with Import, Link_Name => Prefix & "Flush1";
   procedure Flush
    with Import, Link_Name => Prefix & "Flush2";

--     --------------------------------------------
--     -- Specification of line and page lengths --
--     --------------------------------------------
--
--     procedure Set_Line_Length (File : File_Type; To : Count)
--      with Import, Link_Name => Prefix & "Set_Line_Length1";
--     procedure Set_Line_Length (To : Count)
--      with Import, Link_Name => Prefix & "Set_Line_Length2";
--
--     procedure Set_Page_Length (File : File_Type; To : Count)
--      with Import, Link_Name => Prefix & "Set_Page_Length1";
--     procedure Set_Page_Length (To : Count)
--      with Import, Link_Name => Prefix & "Set_Page_Length2";
--
--     function Line_Length (File : File_Type) return Count
--      with Import, Link_Name => Prefix & "Line_Length1";
--     function Line_Length return Count
--      with Import, Link_Name => Prefix & "Line_Length2";
--
--     function Page_Length (File : File_Type) return Count
--      with Import, Link_Name => Prefix & "Page_Length1";
--     function Page_Length return Count
--      with Import, Link_Name => Prefix & "Page_Length2";
--
--     ------------------------------------
--     -- Column, Line, and Page Control --
--     ------------------------------------
--
--     procedure New_Line (File : File_Type; Spacing : Positive_Count := 1);
--     procedure New_Line (Spacing : Positive_Count := 1);
--
--     procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1);
--     procedure Skip_Line (Spacing : Positive_Count := 1);
--
--     function End_Of_Line (File : File_Type) return Boolean;
--     function End_Of_Line return Boolean;
--
--     procedure New_Page (File : File_Type);
--     procedure New_Page;
--
--     procedure Skip_Page (File : File_Type);
--     procedure Skip_Page;
--
--     function End_Of_Page (File : File_Type) return Boolean;
--     function End_Of_Page return Boolean;
--
--     function End_Of_File (File : File_Type) return Boolean;
--     function End_Of_File return Boolean;
--
--     procedure Set_Col (File : File_Type;  To : Positive_Count);
--     procedure Set_Col (To : Positive_Count);
--
--     procedure Set_Line (File : File_Type; To : Positive_Count);
--     procedure Set_Line (To : Positive_Count);
--
--     function Col (File : File_Type) return Positive_Count;
--     function Col return Positive_Count;
--
--     function Line (File : File_Type) return Positive_Count;
--     function Line return Positive_Count;
--
--     function Page (File : File_Type) return Positive_Count;
--     function Page return Positive_Count;
--
--     ----------------------------
--     -- Character Input-Output --
--     ----------------------------
--
--     procedure Get (File : File_Type; Item : out Character);
--     procedure Get (Item : out Character);
--     procedure Put (File : File_Type; Item : Character);
--     procedure Put (Item : Character);
--
--     procedure Look_Ahead
--       (File        : File_Type;
--        Item        : out Character;
--        End_Of_Line : out Boolean);
--
--     procedure Look_Ahead
--       (Item        : out Character;
--        End_Of_Line : out Boolean);
--
--     procedure Get_Immediate
--       (File : File_Type;
--        Item : out Character);
--
--     procedure Get_Immediate
--       (Item : out Character);
--
--     procedure Get_Immediate
--       (File      : File_Type;
--        Item      : out Character;
--        Available : out Boolean);
--
--     procedure Get_Immediate
--       (Item      : out Character;
--        Available : out Boolean);
--
--     -------------------------
--     -- String Input-Output --
--     -------------------------
--
--     procedure Get (File : File_Type; Item : out String);
--     procedure Get (Item : out String);
--     procedure Put (File : File_Type; Item : String);
--     procedure Put (Item : String);
--
--     procedure Get_Line
--       (File : File_Type;
--        Item : out String;
--        Last : out Natural);
--
--     procedure Get_Line
--       (Item : out String;
--        Last : out Natural);
--
--     function Get_Line (File : File_Type) return String;
--
--     function Get_Line return String;
--
--     procedure Put_Line
--       (File : File_Type;
--        Item : String);
--
--     procedure Put_Line
--       (Item : String);
--

   Type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class
     with Storage_Size => 0, Object_Size => 64, Size => 64;
   function Stream (File : File_Type) return Stream_Access
     with Import, Link_Name => Prefix & "Stream";


Private
  Pragma Warnings( Off );

    type File_Type is null record;
    For  File_Type'Size use 64;

  Pragma Warnings( On );
End Testbed.Pure_IO;

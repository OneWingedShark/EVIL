With
Ada.Text_Io.Text_Streams;

Package Testbed.Export_IO is
   Subtype String is Standard.String;
   Prefix : Constant String := "PIO_";

   Package IO renames Ada.Text_IO;
   Package IO_Streams renames IO.Text_Streams;

   Use IO, IO_Streams;

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := ""
     ) with Export, Link_Name => Prefix & "Create1";

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := ""
     ) with Export, Link_Name => Prefix & "Open1";

   procedure Close  (File : in out File_Type)
    with Export, Link_Name => Prefix & "Close1";
   procedure Delete (File : in out File_Type)
    with Export, Link_Name => Prefix & "Delete1";
   procedure Reset  (File : in out File_Type; Mode : File_Mode)
    with Export, Link_Name => Prefix & "Reset1";
   procedure Reset  (File : in out File_Type)
    with Export, Link_Name => Prefix & "Reset2";

   function Mode (File : File_Type) return File_Mode
    with Export, Link_Name => Prefix & "Mode1";
   function Name (File : File_Type) return String
     with Export, Link_Name => Prefix & "Name1";
   function Form (File : File_Type) return String
     with Export, Link_Name => Prefix & "Form1";

   function Is_Open (File : File_Type) return Boolean
    with Export, Link_Name => Prefix & "Is_Open1";

   ------------------------------------------------------
   -- Control of default input, output and error files --
   ------------------------------------------------------

   procedure Set_Input  (File : File_Type)
    with Export, Link_Name => Prefix & "Set_Input1";
   procedure Set_Output (File : File_Type)
    with Export, Link_Name => Prefix & "Set_Output1";
   procedure Set_Error  (File : File_Type)
    with Export, Link_Name => Prefix & "Set_Error1";

   function Standard_Input  return File_Type
    with Export, Link_Name => Prefix & "Standard_Input1";
   function Standard_Output return File_Type
    with Export, Link_Name => Prefix & "Standard_Output1";
   function Standard_Error  return File_Type
    with Export, Link_Name => Prefix & "Standard_Error1";

   function Current_Input  return File_Type
    with Export, Link_Name => Prefix & "Current_Input1";
   function Current_Output return File_Type
    with Export, Link_Name => Prefix & "Current_Outpur1";
   function Current_Error  return File_Type
     with Export, Link_Name => Prefix & "Current_Error1";


   function Standard_Input  return File_Access
    with Export, Link_Name => Prefix & "Standard_Input2";
   function Standard_Output return File_Access
    with Export, Link_Name => Prefix & "Standard_Output2";
   function Standard_Error  return File_Access
    with Export, Link_Name => Prefix & "Standard_Error2";

   function Current_Input  return File_Access
    with Export, Link_Name => Prefix & "Current_Input2";
   function Current_Output return File_Access
    with Export, Link_Name => Prefix & "Current_Output2";
   function Current_Error  return File_Access
    with Export, Link_Name => Prefix & "Current_Error2";

   --------------------
   -- Buffer control --
   --------------------

   procedure Flush (File : File_Type)
    with Export, Link_Name => Prefix & "Flush1";
   procedure Flush
    with Export, Link_Name => Prefix & "Flush2";

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (File : File_Type; To : Count)
    with Export, Link_Name => Prefix & "Set_Line_Length1";
   procedure Set_Line_Length (To : Count)
    with Export, Link_Name => Prefix & "Set_Line_Length2";

   procedure Set_Page_Length (File : File_Type; To : Count)
    with Export, Link_Name => Prefix & "Set_Page_Length1";
   procedure Set_Page_Length (To : Count)
    with Export, Link_Name => Prefix & "Set_Page_Length2";

   function Line_Length (File : File_Type) return Count
    with Export, Link_Name => Prefix & "Line_Length1";
   function Line_Length return Count
    with Export, Link_Name => Prefix & "Line_Length2";

   function Page_Length (File : File_Type) return Count
    with Export, Link_Name => Prefix & "Page_Length1";
   function Page_Length return Count
    with Export, Link_Name => Prefix & "Page_Length2";

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1)
    with Export, Link_Name => Prefix & "New_Line1";
   procedure New_Line (Spacing : Positive_Count := 1)
    with Export, Link_Name => Prefix & "New_Line2";

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1)
    with Export, Link_Name => Prefix & "Skip_Line1";
   procedure Skip_Line (Spacing : Positive_Count := 1)
    with Export, Link_Name => Prefix & "Skip_Line2";

   function End_Of_Line (File : File_Type) return Boolean
    with Export, Link_Name => Prefix & "End_Of_Line1";
   function End_Of_Line return Boolean
    with Export, Link_Name => Prefix & "End_Of_Line2";

   procedure New_Page (File : File_Type)
    with Export, Link_Name => Prefix & "New_Page1";
   procedure New_Page
    with Export, Link_Name => Prefix & "New_Page2";

   procedure Skip_Page (File : File_Type)
    with Export, Link_Name => Prefix & "Skip_Page1";
   procedure Skip_Page
    with Export, Link_Name => Prefix & "Skip_Page2";

   function End_Of_Page (File : File_Type) return Boolean
    with Export, Link_Name => Prefix & "End_Of_Page1";
   function End_Of_Page return Boolean
    with Export, Link_Name => Prefix & "End_Of_Page2";

   function End_Of_File (File : File_Type) return Boolean
    with Export, Link_Name => Prefix & "End_Of_File1";
   function End_Of_File return Boolean
    with Export, Link_Name => Prefix & "End_Of_File2";

   procedure Set_Col (File : File_Type;  To : Positive_Count)
    with Export, Link_Name => Prefix & "Set_Col1";
   procedure Set_Col (To : Positive_Count)
    with Export, Link_Name => Prefix & "Set_Col2";

   procedure Set_Line (File : File_Type; To : Positive_Count)
    with Export, Link_Name => Prefix & "Set_Line1";
   procedure Set_Line (To : Positive_Count)
    with Export, Link_Name => Prefix & "Set_Line2";

   function Col (File : File_Type) return Positive_Count
    with Export, Link_Name => Prefix & "Col1";
   function Col return Positive_Count
    with Export, Link_Name => Prefix & "Col2";

   function Line (File : File_Type) return Positive_Count
    with Export, Link_Name => Prefix & "Line1";
   function Line return Positive_Count
    with Export, Link_Name => Prefix & "Line2";

   function Page (File : File_Type) return Positive_Count
    with Export, Link_Name => Prefix & "Page1";
   function Page return Positive_Count
    with Export, Link_Name => Prefix & "Page2";

   ----------------------------
   -- Character Input-Output --
   ----------------------------

   procedure Get (File : File_Type; Item : out Character)
    with Export, Link_Name => Prefix & "Get1";
   procedure Get (Item : out Character)
    with Export, Link_Name => Prefix & "Get2";
   procedure Put (File : File_Type; Item : Character)
    with Export, Link_Name => Prefix & "Put1";
   procedure Put (Item : Character)
    with Export, Link_Name => Prefix & "Put2";

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Character;
      End_Of_Line : out Boolean
     ) with Export, Link_Name => Prefix & "Look_Ahead1";

   procedure Look_Ahead
     (Item        : out Character;
      End_Of_Line : out Boolean
     ) with Export, Link_Name => Prefix & "Look_Ahead2";

   procedure Get_Immediate
     (File : File_Type;
      Item : out Character
     ) with Export, Link_Name => Prefix & "Get_Immediate1";

   procedure Get_Immediate
     (Item : out Character
     ) with Export, Link_Name => Prefix & "Get_Immediate2";

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Character;
      Available : out Boolean
     ) with Export, Link_Name => Prefix & "Get_Immediate3";

   procedure Get_Immediate
     (Item      : out Character;
      Available : out Boolean
     ) with Export, Link_Name => Prefix & "Get_Immediate4";

   -------------------------
   -- String Input-Output --
   -------------------------

   procedure Get (File : File_Type; Item : out String)
    with Export, Link_Name => Prefix & "Get3";
   procedure Get (Item : out String)
    with Export, Link_Name => Prefix & "Get4";
   procedure Put (File : File_Type; Item : String)
    with Export, Link_Name => Prefix & "Put3";
   procedure Put (Item : String)
    with Export, Link_Name => Prefix & "Put4";

   procedure Get_Line
     (File : File_Type;
      Item : out String;
      Last : out Natural
     ) with Export, Link_Name => Prefix & "Get_Line1";

   procedure Get_Line
     (Item : out String;
      Last : out Natural
     ) with Export, Link_Name => Prefix & "Get_Line2";

   function Get_Line (File : File_Type) return String
    with Export, Link_Name => Prefix & "Get_Line3";

   function Get_Line return String
    with Export, Link_Name => Prefix & "Get_Line4";

   procedure Put_Line
     (File : File_Type;
      Item : String
     ) with Export, Link_Name => Prefix & "Put_Line1";

   procedure Put_Line
     (Item : String)
    with Export, Link_Name => Prefix & "Put_Line2";

   ----------------
   -- Exceptions --
   ----------------
--
--     Status_Error : exception renames IO.Status_Error;
--     Mode_Error   : exception renames IO.Mode_Error;
--     Name_Error   : exception renames IO.Name_Error;
--     Use_Error    : exception renames IO.Use_Error;
--     Device_Error : exception renames IO.Device_Error;
--     End_Error    : exception renames IO.End_Error;
--     Data_Error   : exception renames IO.Data_Error;
--     Layout_Error : exception renames IO.Layout_Error;


   subtype Stream_Access is IO_Streams.Stream_Access;
   function Stream (File : File_Type) return Stream_Access
     with Export, Link_Name => Prefix & "Stream";

Private
   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := ""
     ) renames IO.Create;

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := ""
     ) renames IO.Open;

   procedure Close  (File : in out File_Type)
    renames IO.Close;
   procedure Delete (File : in out File_Type)
    renames IO.Delete;
   procedure Reset  (File : in out File_Type; Mode : File_Mode)
    renames IO.Reset;
   procedure Reset  (File : in out File_Type)
    renames IO.Reset;

   function Mode (File : File_Type) return File_Mode
    renames IO.Mode;
   function Name (File : File_Type) return String
     renames IO.Name;
   function Form (File : File_Type) return String
     renames IO.Form;

   function Is_Open (File : File_Type) return Boolean
    renames IO.Is_Open;

   ------------------------------------------------------
   -- Control of default input, output and error files --
   ------------------------------------------------------

   procedure Set_Input  (File : File_Type)
    renames IO.Set_Input;
   procedure Set_Output (File : File_Type)
    renames IO.Set_Output;
   procedure Set_Error  (File : File_Type)
    renames IO.Set_Error;

   function Standard_Input  return File_Type
    renames IO.Standard_Input;
   function Standard_Output return File_Type
    renames IO.Standard_Output;
   function Standard_Error  return File_Type
    renames IO.Standard_Error;

   function Current_Input  return File_Type
    renames IO.Current_Input;
   function Current_Output return File_Type
    renames IO.Current_Output;
   function Current_Error  return File_Type
    renames IO.Current_Error;


   function Standard_Input  return File_Access
    renames IO.Standard_Input;
   function Standard_Output return File_Access
    renames IO.Standard_Output;
   function Standard_Error  return File_Access
    renames IO.Standard_Error;

   function Current_Input  return File_Access
    renames IO.Current_Input;
   function Current_Output return File_Access
    renames IO.Current_Output;
   function Current_Error  return File_Access
    renames IO.Current_Error;

   --------------------
   -- Buffer control --
   --------------------

   procedure Flush (File : File_Type)
    renames IO.Flush;
   procedure Flush
    renames IO.Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (File : File_Type; To : Count)
    renames IO.Set_Line_Length;
   procedure Set_Line_Length (To : Count)
    renames IO.Set_Line_Length;

   procedure Set_Page_Length (File : File_Type; To : Count)
    renames IO.Set_Page_Length;
   procedure Set_Page_Length (To : Count)
    renames IO.Set_Page_Length;

   function Line_Length (File : File_Type) return Count
    renames IO.Line_Length;
   function Line_Length return Count
    renames IO.Line_Length;

   function Page_Length (File : File_Type) return Count
    renames IO.Page_Length;
   function Page_Length return Count
    renames IO.Page_Length;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1)
    renames IO.New_Line;
   procedure New_Line (Spacing : Positive_Count := 1)
    renames IO.New_Line;

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1)
    renames IO.Skip_Line;
   procedure Skip_Line (Spacing : Positive_Count := 1)
    renames IO.Skip_Line;

   function End_Of_Line (File : File_Type) return Boolean
    renames IO.End_Of_Line;
   function End_Of_Line return Boolean
    renames IO.End_Of_Line;

   procedure New_Page (File : File_Type)
    renames IO.New_Page;
   procedure New_Page
    renames IO.New_Page;

   procedure Skip_Page (File : File_Type)
    renames IO.Skip_Page;
   procedure Skip_Page
    renames IO.Skip_Page;

   function End_Of_Page (File : File_Type) return Boolean
    renames IO.End_Of_Page;
   function End_Of_Page return Boolean
    renames IO.End_Of_Page;

   function End_Of_File (File : File_Type) return Boolean
    renames IO.End_Of_File;
   function End_Of_File return Boolean
    renames IO.End_Of_File;

   procedure Set_Col (File : File_Type;  To : Positive_Count)
    renames IO.Set_Col;
   procedure Set_Col (To : Positive_Count)
    renames IO.Set_Col;

   procedure Set_Line (File : File_Type; To : Positive_Count)
    renames IO.Set_Line;
   procedure Set_Line (To : Positive_Count)
    renames IO.Set_Line;

   function Col (File : File_Type) return Positive_Count
    renames IO.Col;
   function Col return Positive_Count
    renames IO.Col;

   function Line (File : File_Type) return Positive_Count
    renames IO.Line;
   function Line return Positive_Count
    renames IO.Line;

   function Page (File : File_Type) return Positive_Count
    renames IO.Page;
   function Page return Positive_Count
    renames IO.Page;

   ----------------------------
   -- Character Input-Output --
   ----------------------------

   procedure Get (File : File_Type; Item : out Character)
    renames IO.Get;
   procedure Get (Item : out Character)
    renames IO.Get;
   procedure Put (File : File_Type; Item : Character)
    renames IO.Put;
   procedure Put (Item : Character)
    renames IO.Put;

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Character;
      End_Of_Line : out Boolean
     ) renames IO.Look_Ahead;

   procedure Look_Ahead
     (Item        : out Character;
      End_Of_Line : out Boolean
     ) renames IO.Look_Ahead;

   procedure Get_Immediate
     (File : File_Type;
      Item : out Character
     ) renames IO.Get_Immediate;

   procedure Get_Immediate
     (Item : out Character
     ) renames IO.Get_Immediate;

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Character;
      Available : out Boolean
     ) renames IO.Get_Immediate;

   procedure Get_Immediate
     (Item      : out Character;
      Available : out Boolean
     ) renames IO.Get_Immediate;

   -------------------------
   -- String Input-Output --
   -------------------------

   procedure Get (File : File_Type; Item : out String)
    renames IO.Get;
   procedure Get (Item : out String)
    renames IO.Get;
   procedure Put (File : File_Type; Item : String)
    renames IO.Put;
   procedure Put (Item : String)
    renames IO.Put;

   procedure Get_Line
     (File : File_Type;
      Item : out String;
      Last : out Natural
     ) renames IO.Get_Line;

   procedure Get_Line
     (Item : out String;
      Last : out Natural
     ) renames IO.Get_Line;

   function Get_Line (File : File_Type) return String
    renames IO.Get_Line;

   function Get_Line return String
    renames IO.Get_Line;

   procedure Put_Line
     (File : File_Type;
      Item : String
     ) renames IO.Put_Line;

   procedure Put_Line
     (Item : String)
    renames IO.Put_Line;

   function Stream (File : File_Type) return Stream_Access
       renames IO_Streams.Stream;

End Testbed.Export_IO;

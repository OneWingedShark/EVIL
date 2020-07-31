Pragma Ada_2012;
Pragma Assertion_Policy( Check );

Package Body EVIL.Util.Files with SPARK_Mode => On is

    --------------
    -- GENERICS --
    --------------

    Generic
	with Procedure Init_File(
	    File : in out File_Type;
	    Mode : File_Mode;
	    Name : String := Empty_String;
	    Form : String := Empty_String
	   );
    Function Generic_Make( Name : String; Mode : File_Mode ) return File;
    Function Generic_Make( Name : String; Mode : File_Mode ) return File is
	Use Ada.Finalization;
    Begin
	Return Result : File := (Limited_Controlled with Others => <>) do
	    Init_File(
		File => Result.Data,
		Mode => Mode,
		Name => Name
	       );
	    Result.FSA:= Stream(Result.Data);
	End return;
    End;

    Generic
	Type Return_Type(<>) is private;
	with Function Op( File : File_Type ) return Return_Type;
    Function Generic_Function( Object : File ) return Return_Type with Inline;
    Function Generic_Function( Object : File ) return Return_Type is
      ( Op(Object.Data) );

    Generic
	with Procedure Op( File : in out File_Type );
    Procedure Generic_FileOp( Object : in out File ) with Inline;
    Procedure Generic_FileOp( Object : in out File ) is
    Begin
	Op( Object.Data );
    End Generic_FileOp;

    --------------------
    -- INSTANTIATIONS --
    --------------------

    Function Create_File is new Generic_Make( Init_File => Create  );
    Function Open_File   is new Generic_Make( Init_File => Open    );
    Function Get_Mode    is new Generic_Function( File_Mode, Mode  );
    Function Get_Name    is new Generic_Function( String, Name     );
    Function Get_Form    is new Generic_Function( String, Form     );
    Function Get_Open    is new Generic_Function( Boolean, Is_Open );
    Procedure Do_Close   is new Generic_FileOp  ( Close            );
    Procedure Do_Delete  is new Generic_FileOp  ( Delete           );
    Procedure Do_Reset   is new Generic_FileOp  ( Reset            );


    -------------
    -- RENAMES --
    -------------

    Function Create( Name : String; Mode : File_Mode ) return File
    renames Create_File;

    Function Open( Name : String; Mode : File_Mode ) return File
    renames Open_File;


    Function Mode  ( Object : File ) return File_Mode	renames Get_Mode;
    Function Name  ( Object : File ) return String	renames Get_Name;
    Function Form  ( Object : File ) return String	renames Get_Form;
    Function Open  ( Object : File ) return Boolean	renames Get_Open;

    Procedure Close  (Object  : in out File)		renames Do_Close;
    Procedure Delete (Object  : in out File)		renames Do_Delete;
    Procedure Reset  (Object  : in out File)		renames Do_Reset;

    ------------------
    -- FINALIZATION --
    ------------------
    Procedure Finalize (Object : in out File) is
    Begin
	Close( Object.Data );
    End Finalize;

    -----------------
    -- non-renames --
    -----------------

    Procedure Reset  (Object  : in out File; Mode : File_Mode) is
    Begin
	Reset(File => Object.Data, Mode => Mode);
    End Reset;


End EVIL.Util.Files;

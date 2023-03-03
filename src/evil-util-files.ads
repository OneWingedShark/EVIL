Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
EVIL.Util.Strings,
Ada.Finalization,
Ada.Streams;

-- EVIL.Util.Files
--	This package is to provide a consistent interface for file-handling,
--	thus, provided a dependency on one of the IO packages of Ada and the
--	appropriate FILE_TYPE/FILE_MODE/STREAM_ACCESS, and STRING-package the
--	instantiation of this package may be used to reduce dependencies when
--	altering the parameters.
--
-- The FILE type is controlled for the convienience that the finalization of the
-- type causes the closing of the file.
Generic
   with Package S is new EVIL.Util.Strings(<>);


    Type File_Type is limited private;
    Type File_Mode is (<>);
    Type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

    with function  Stream (File : File_Type) return Stream_Access     is <>;
    with procedure Create
      (File : in out File_Type;
       Mode : File_Mode;
       Name : S.String := S.Empty_String;
       Form : S.String := S.Empty_String)                             is <>;

    with procedure Open
      (File : in out File_Type;
       Mode : File_Mode;
       Name : S.String;
       Form : S.String := S.Empty_String)                             is <>;

    with procedure Close  (File : In Out File_Type)                   is <>;
    with procedure Delete (File : In Out File_Type)                   is <>;
    with procedure Reset  (File : In Out File_Type; Mode : File_Mode) is <>;
    with procedure Reset  (File : In Out File_Type)                   is <>;

    with function  Mode   (File : In     File_Type) return File_Mode  is <>;
    with function  Name   (File : In     File_Type) return S.String   is <>;
    with function  Form   (File : In     File_Type) return S.String   is <>;

    with function Is_Open (File : In     File_Type) return Boolean    is <>;

Package EVIL.Util.Files with Pure, SPARK_Mode => On is

   Type File is tagged limited private;

   Function  Create (Name    : In     S.String; Mode : In File_Mode) Return File
   with Global => Null, Depends => (Create'Result => (Name, Mode));

   Function  Open   (Name    : In     S.String; Mode : In File_Mode) Return File
   with Global => Null, Depends => (Open'Result => (Name, Mode));

   Function  Mode   (Object  : In     File) Return File_Mode
   with Global => Null, Depends => (Mode'Result => Object);

   Function  Name   (Object  : In     File) Return S.String
   with Global => Null, Depends => (Name'Result => Object);

   Function  Form   (Object  : In     File) Return S.String
   with Global => Null, Depends => (Form'Result => Object);

   Function  Open   (Object  : In     File) Return Boolean
   with Global => Null, Depends => (Open'Result => Object);

   Function  Stream (Object  : In     File) Return Stream_Access
   with Global => Null, Depends => (Stream'Result => Object);

   Procedure Close  (Object  : In Out File)
   with Global => Null, Depends => (Object =>+ Null);

   Procedure Delete (Object  : In Out File)
   with Global => Null, Depends => (Object =>+ Null);

   Procedure Reset  (Object  : In Out File)
   with Global => Null, Depends => (Object =>+ Null);

   Procedure Reset  (Object  : In Out File; Mode : In File_Mode)
   with Global => Null, Depends => (Object =>+ Mode);

Private

   -- We Construct the FILE type as the derivation of the limited-controlled
   -- abstract type, and two values of the types passed via formal-parameters:
   -- a FILE_TYPE and a STREAM_ACCESS type, which paired provide the interfacing
   -- to the subprograms exposed in the IO-package used to instantiate this.
    Type File is new Ada.Finalization.Limited_Controlled with Record
	Data : Aliased File_Type;
	FSA  : Stream_Access;
    end record;

   -- Close the file when this object finalizes.
    Overriding
    Procedure Finalize (Object : In Out File)
      with Global => Null, Depends => (Object => Null);

   -- Obtain the stream associated with the File.
    Function Stream  (Object  : File)  return Stream_Access is (Object.FSA);
End EVIL.Util.Files;

with Ahven;
Package Body Testbed.Tests with SPARK_Mode => On is

   procedure Initialize (T : in out Test)  is
      Pragma SPARK_Mode( OFF );
	begin
	    Set_Name (T, "My tests");

	    Ahven.Framework.Add_Test_Routine
	      (T, My_First_Test'Access, "My first test");
	end Initialize;

    procedure My_First_Test is
    begin
	Ahven.Assert (1 /= 4, "1 /= 4 failed!");
    End My_First_Test;

End Testbed.Tests;

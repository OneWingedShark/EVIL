With
Ahven.Framework;

Package Testbed.Tests with SPARK_Mode => On, Elaborate_Body is
   Type Test;


	type Test is new Ahven.Framework.Test_Case with null record;

Pragma Warnings(Off);
   procedure Initialize (T : in out Test)
   with SPARK_Mode => Off;

	procedure My_First_Test;
Pragma Warnings(On);
End Testbed.Tests;

-----------------------------------------------------------------------
--  EL.Objects.Time.Tests - Testsuite time objects
--  Copyright (C) 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package EL.Objects.Time.Tests is

   procedure Add_Tests (Suite : Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Time_Object (T : in out Test);
   procedure Test_Time_To_String (T : in out Test);
   procedure Test_Time_Add (T : in out Test);

end EL.Objects.Time.Tests;

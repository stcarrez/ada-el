-----------------------------------------------------------------------
--  EL.Beans.Tests - Testsuite for EL.Beans
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package EL.Beans.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test Add_Parameter
   procedure Test_Add_Parameter (T : in out Test);

   --  Test the Initialize procedure with a set of expressions
   procedure Test_Initialize (T : in out Test);

end EL.Beans.Tests;

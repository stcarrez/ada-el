-----------------------------------------------------------------------
--  el-utils-tests - Tests the EL utils
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package EL.Utils.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Expand_Properties (T : in out Test);

   procedure Test_Expand_Recursion (T : in out Test);

   procedure Test_Eval (T : in out Test);

end EL.Utils.Tests;

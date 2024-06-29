-----------------------------------------------------------------------
--  el-contexts-tests - Tests the EL contexts
--  Copyright (C) 2011, 2015, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package EL.Contexts.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Context_Properties (T : in out Test);

   --  Test the thread local EL context.
   procedure Test_Context_TLS (T : in out Test);

   --  Test the EL guarded context.
   procedure Test_Guarded_Context (T : in out Test);

   --  Test the default EL resolver.
   procedure Test_Resolver_Context (T : in out Test);

end EL.Contexts.Tests;

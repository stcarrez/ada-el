-----------------------------------------------------------------------
--  EL testsuite - EL Testsuite
--  Copyright (C) 2009, 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package EL.Testsuite is

   function Suite return Util.Tests.Access_Test_Suite;

   type Test is new Util.Tests.Test with record
      I1 : Integer;
      I2 : Integer;
   end record;

   --  Test object integer
   procedure Test_To_Object_Integer (T : in out Test);

   --  Test object integer
   procedure Test_Expression (T : in out Test);

end EL.Testsuite;

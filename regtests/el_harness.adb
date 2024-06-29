-----------------------------------------------------------------------
--  EL -- Expression Language
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with EL.Testsuite;
with Util.Tests;

procedure EL_Harness is

   procedure Harness is new Util.Tests.Harness (EL.Testsuite.Suite);
begin
   Harness ("el-tests.xml");
end EL_Harness;

-----------------------------------------------------------------------
--  EL testsuite - EL Testsuite
--  Copyright (C) 2009, 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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

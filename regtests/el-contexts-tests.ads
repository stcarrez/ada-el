-----------------------------------------------------------------------
--  el-contexts-tests - Tests the EL contexts
--  Copyright (C) 2011, 2015, 2022 Stephane Carrez
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

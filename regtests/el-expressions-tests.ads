-----------------------------------------------------------------------
--  el-expressions-tests - EL Testsuite
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
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
with EL.Contexts.Default;
package EL.Expressions.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      Context : EL.Contexts.Default.Default_Context_Access;
   end record;

   --  Set up performed before each test case
   overriding
   procedure Set_Up (T : in out Test);

   --  Tear down performed after each test case
   overriding
   procedure Tear_Down (T : in out Test);

   procedure Test_Bean_Evaluation (T : in out Test);
   procedure Test_Parse_Error (T : in out Test);
   procedure Test_Method_Evaluation (T : in out Test);
   procedure Test_Invalid_Method (T : in out Test);
   procedure Test_Simple_Evaluation (T : in out Test);
   procedure Test_Function_Evaluation (T : in out Test);
   procedure Test_Object_Sizes (T : in out Test);

   --  Test to verify the Is_Valid operation
   procedure Test_Method_Is_Valid (T : in out Test);

   --  Test function namespace
   procedure Test_Function_Namespace (T : in out Test);

   --  Test the use of a value expression.
   procedure Test_Value_Expression (T : in out Test);

   --  Test the invalid value expression
   procedure Test_Invalid_Value_Expression (T : in out Test);

   procedure Check (T      : in out Test;
                    Expr   : in String;
                    Expect : in String);

   --  Test some reductions.
   procedure Test_Reduce_Expression (T : in out Test);

   procedure Test_Reduce_Expression_Variable (T : in out Test);

   --  Test the method expression.
   procedure Test_Method_Expression (T : in out Test);

   --  Test calling Get_Value or Set_Value on un-initialized/empty expression.
   procedure Test_No_Expression (T : in out Test);

end EL.Expressions.Tests;

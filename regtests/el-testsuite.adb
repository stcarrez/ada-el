-----------------------------------------------------------------------
--  EL testsuite - EL Testsuite
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Util.Test_Caller;
with EL.Expressions;
with EL.Objects;
with EL.Contexts;
with EL.Contexts.Default;
with EL.Expressions.Tests;
with EL.Beans.Tests;
with EL.Contexts.Tests;
with EL.Utils.Tests;
package body EL.Testsuite is

   use EL.Objects;

   --  ------------------------------
   --  Test object integer
   --  ------------------------------
   procedure Test_To_Object_Integer (T : in out Test) is
   begin
      declare
         Value : constant EL.Objects.Object := To_Object (Integer (1));
      begin
         T.Assert (Condition => To_Integer (Value) = 1,
                   Message => "Object.To_Integer: invalid integer returned");
         T.Assert (Condition => To_Long_Integer (Value) = 1,
                   Message => "Object.To_Long_Integer: invalid integer returned");
         T.Assert (Condition => To_Boolean (Value),
                   Message => "Object.To_Boolean: invalid return");

         declare
            V2 : constant EL.Objects.Object := Value + To_Object (Long_Integer (100));
         begin
            T.Assert (Condition => To_Integer (V2) = 101,
                      Message => "To_Integer invalid after an add");
         end;
      end;
   end Test_To_Object_Integer;

   --  ------------------------------
   --  Test object integer
   --  ------------------------------
   procedure Test_Expression (T : in out Test) is
      E     : EL.Expressions.Expression;
      Value : Object;
      Ctx   : EL.Contexts.Default.Default_Context;
   begin
      --  Positive number
      E := EL.Expressions.Create_Expression ("12345678901", Ctx);
      Value := E.Get_Value (Ctx);
      T.Assert (Condition => To_Long_Long_Integer (Value) = Long_Long_Integer (12345678901),
                Message => "[1] Expression result invalid: " & To_String (Value));

      --  Negative number
      E := EL.Expressions.Create_Expression ("-10", Ctx);
      Value := E.Get_Value (Ctx);
      T.Assert (Condition => To_Integer (Value) = -10,
                Message => "[2] Expression result invalid: " & To_String (Value));

      --  Simple add
      E := EL.Expressions.Create_Expression ("#{1+1}", Ctx);
      Value := E.Get_Value (Ctx);
      T.Assert (Condition => To_Integer (Value) = 2,
                Message => "[2.1] Expression result invalid: " & To_String (Value));

      --  Constant expressions
      E := EL.Expressions.Create_Expression ("#{12 + (123 - 3) * 4}", Ctx);
      Value := E.Get_Value (Ctx);
      T.Assert (Condition => To_Integer (Value) = 492,
                Message => "[3] Expression result invalid: " & To_String (Value));

      --  Constant expressions
      E := EL.Expressions.Create_Expression ("#{12 + (123 - 3) * 4 + (23? 10 : 0)}", Ctx);
      Value := E.Get_Value (Ctx);
      T.Assert (Condition => To_Integer (Value) = 502,
                Message => "[4] Expression result invalid: " & To_String (Value));

      --  Choice expressions
      E := EL.Expressions.Create_Expression ("#{1 > 2 ? 12 + 2 : 3 * 3}", Ctx);
      Value := E.Get_Value (Ctx);
      T.Assert (Condition => To_Integer (Value) = 9,
                Message => "[5] Expression result invalid: " & To_String (Value));

      --  Choice expressions using strings
      E := EL.Expressions.Create_Expression ("#{1 > 2 ? 12 + 2 : 'A string'}", Ctx);
      Value := E.Get_Value (Ctx);
      T.Assert (Condition => To_String (Value) = "A string",
                Message => "[6] Expression result invalid: " & To_String (Value));

   end Test_Expression;

   package Caller is new Util.Test_Caller (Test, "EL");

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      Caller.Add_Test (Ret, "Test To_Object (Integer)",
                       Test_To_Object_Integer'Access);
      Caller.Add_Test (Ret, "Test Expressions", Test_Expression'Access);
      EL.Expressions.Tests.Add_Tests (Ret);
      EL.Contexts.Tests.Add_Tests (Ret);
      EL.Beans.Tests.Add_Tests (Ret);
      EL.Utils.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end EL.Testsuite;

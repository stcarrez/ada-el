-----------------------------------------------------------------------
--  EL testsuite - EL Testsuite
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with AUnit.Test_Caller;
with AUnit.Assertions;
with EL.Expressions;
with Test_Bean;
package body EL.Expressions.Tests is

   use Test_Bean;
   use EL.Expressions;
   use AUnit.Assertions;
   use AUnit.Test_Fixtures;

   procedure Check_Error (T    : in out Test'Class;
                          Expr : in String);

   --  Check that evaluating an expression raises an exception
   procedure Check_Error (T    : in out Test'Class;
                          Expr : in String) is
      E : Expression;
   begin
      E := Create_Expression (Context => T.Context, Expr => Expr);

      pragma Unreferenced (E);
      Assert (Condition => False,
              Message => "Evaludation of '" & Expr & "' should raise an exception");
   exception
      when Invalid_Expression =>
         null;
   end Check_Error;

   --  Check that evaluating an expression returns the expected result
   --  (to keep the test simple, results are only checked using strings)
   procedure Check (T      : in out Test;
                    Expr   : in String;
                    Expect : in String) is
      E : constant Expression := Create_Expression (Context => T.Context,
                                                    Expr => Expr);
      V : constant Object := E.Get_Value (Context => T.Context);
   begin
      Assert (Condition => To_String (V) = Expect,
              Message => "Evaluate '" & Expr & "' returned '" & To_String (V)
              & "' when we expect '" & Expect & "'");

      declare
         E2 : constant Expression := E.Reduce_Expression (Context => T.Context);
         V2 : constant Object := E2.Get_Value (Context => T.Context);
      begin
         Assert (To_String (V2) = Expect,
                 "Reduce produced incorrect result: " & To_String (V2));
      end;

   end Check;

   --  Test evaluation of expression using a bean
   procedure Test_Bean_Evaluation (T : in out Test) is
      P : constant Person_Access := Create_Person ("Joe", "Black", 42);
   begin
      T.Context.Set_Variable ("user", P);

      Check (T, "#{empty user}", "FALSE");
      Check (T, "#{not empty user}", "TRUE");
      Check (T, "#{user.firstName}", "Joe");
      Check (T, "#{user.lastName}", "Black");
      Check (T, "#{user.age}", " 42");
      Check (T, "#{user.date}", To_String (To_Object (P.Date)));
      Check (T, "#{user.weight}", To_String (To_Object (P.Weight)));

      P.Age := P.Age + 1;
      Check (T, "#{user.age}", " 43");
      Check (T, "#{user.firstName & ' ' & user.lastName}", "Joe Black");

      Check (T, "Joe is#{user.age} year#{user.age > 0 ? 's' : ''} old",
             "Joe is 43 years old");
   end Test_Bean_Evaluation;

   --  Test evaluation of expression using a bean
   procedure Test_Parse_Error (T : in out Test) is
   begin
      Check_Error (T, "#{1 +}");
      Check_Error (T, "#{12(}");
      Check_Error (T, "#{foo(1)}");
      Check_Error (T, "#{1+2+'abc}");
      Check_Error (T, "#{1+""}");
      Check_Error (T, "#{12");
      Check_Error (T, "${1");
      Check_Error (T, "test #{'}");
   end Test_Parse_Error;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      --  Test_Bean verifies several methods.  Register several times
      --  to enumerate what is tested.
      Suite.Add_Test (Caller.Create ("Test EL.Contexts.Set_Variable",
                                     Test_Bean_Evaluation'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Beans.Get_Value",
                                     Test_Bean_Evaluation'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Expressions.Create_Expression",
                                     Test_Bean_Evaluation'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Expressions.Get_Value",
                                     Test_Bean_Evaluation'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Expressions.Create_Expression (Parse Error)",
                                     Test_Parse_Error'Access));

   end Add_Tests;

end EL.Expressions.Tests;

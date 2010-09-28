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

with EL.Functions;
with EL.Functions.Default;
with EL.Expressions;
with Test_Bean;
with Action_Bean;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
package body EL.Expressions.Tests is

   use Test_Bean;
   use EL.Expressions;
   use AUnit.Assertions;
   use AUnit.Test_Fixtures;
   use Ada.Strings.Unbounded;

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
   procedure Test_Simple_Evaluation (T : in out Test) is
   begin
      Check (T, "#{true}", "TRUE");
      Check (T, "#{false}", "FALSE");
      Check (T, "#{not false}", "TRUE");
      Check (T, "#{! false}", "TRUE");
      Check (T, "#{4 * 3}", "12");
      Check (T, "#{12 / 3}", "4");
      Check (T, "#{12 div 3}", "4");
      Check (T, "#{3 % 2}", "1");
      Check (T, "#{3 mod 2}", "1");
      Check (T, "#{3 <= 2}", "FALSE");
      Check (T, "#{3 < 2}", "FALSE");
      Check (T, "#{3 > 2}", "TRUE");
      Check (T, "#{3 >= 2}", "TRUE");
      Check (T, "#{3 >= 2 && 2 >= 3}", "FALSE");
      Check (T, "#{3 >= 4 || 2 < 3}", "TRUE");
      Check (T, "#{3 > 4 ? 1 : 2}", "2");
      Check (T, "#{3 < 4 ? 1 : 2}", "1");
      Check (T, "#{true and false}", "FALSE");
      Check (T, "#{true or false}", "TRUE");
      Check (T, "#{- 23}", "-23");
      Check (T, "#{1.0}", "1");
      Check (T, "#{'a\'b'}", "a'b");
      Check (T, "'a'", "'a'");
      Check (T, "\#{test}", "#{test}");
      Check (T, "\${test}", "${test}");
   end Test_Simple_Evaluation;

   --  Test evaluation of expression using a bean
   procedure Test_Bean_Evaluation (T : in out Test) is
      P : constant Person_Access := Create_Person ("Joe", "Black", 42);
   begin
      T.Context.Set_Variable ("user", P);

      Check (T, "#{user ne null}", "TRUE");
      Check (T, "#{empty user}", "FALSE");
      Check (T, "#{not empty user}", "TRUE");
      Check (T, "#{user.firstName}", "Joe");
      Check (T, "#{user.lastName}", "Black");
      Check (T, "#{user.age}", "42");
      Check (T, "#{user.age le 42}", "TRUE");
      Check (T, "#{user.age lt 44}", "TRUE");
      Check (T, "#{user.age ge 42}", "TRUE");
      Check (T, "#{user.age ge 45}", "FALSE");
      Check (T, "#{user.age gt 42}", "FALSE");
      Check (T, "#{user.date}", To_String (To_Object (P.Date)));
      Check (T, "#{user.weight}", To_String (To_Object (P.Weight)));

      P.Age := P.Age + 1;
      Check (T, "#{user.age}", "43");
      Check (T, "#{user.firstName & ' ' & user.lastName}", "Joe Black");

      Check (T, "#{user.firstName eq 'Joe'}", "TRUE");
      Check (T, "#{user.firstName ne 'Joe'}", "FALSE");
      Check (T, "#{user.firstName eq 'Boe' or user.firstName eq 'Joe'}", "TRUE");
      Check (T, "Joe is #{user.age} year#{user.age > 0 ? 's' : ''} old",
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
      Check_Error (T, "#{12 > 23 ? 44}");
      Check_Error (T, "#{");
      Check_Error (T, "${(12+1}");
      Check_Error (T, "#{'a\\'b'}");
      Check_Error (T, "#");
      Check_Error (T, "$");
   end Test_Parse_Error;

   --  Function to format a string
   function Format1 (Arg : EL.Objects.Object) return EL.Objects.Object is
      S : constant String := To_String (Arg);
   begin
      return To_Object ("[" & S & "]");
   end Format1;

   --  Function to format a string
   function Format2 (Arg1, Arg2 : EL.Objects.Object) return EL.Objects.Object is
      S1 : constant String := To_String (Arg1);
      S2 : constant String := To_String (Arg2);
      begin
      return To_Object ("[" & S1 & "-" & S2 & "]");
   end Format2;

   --  Function to format a string
   function Format3 (Arg1, Arg2, Arg3 : EL.Objects.Object) return EL.Objects.Object is
      S1 : constant String := To_String (Arg1);
      S2 : constant String := To_String (Arg2);
      S3 : constant String := To_String (Arg3);
   begin
      return To_Object ("[" & S1 & "-" & S2 & "-" & S3 & "]");
   end Format3;

   --  Test evaluation of expression using a bean
   procedure Test_Function_Evaluation (T : in out Test) is
      P : constant Person_Access := Create_Person ("Joe", "Black", 42);
      Fn  : constant EL.Functions.Function_Mapper_Access
        := new EL.Functions.Default.Default_Function_Mapper;
      E : EL.Expressions.Expression;
      Result : EL.Objects.Object;
   begin
      T.Context.Set_Variable ("user", P);

      --  Register the 'format' function.
      Fn.Set_Function (Namespace => "fn",
                       Name      => "format1",
                       Func      => Format1'Access);
      Fn.Set_Function (Namespace => "fn",
                       Name      => "format2",
                       Func      => Format2'Access);
      Fn.Set_Function (Namespace => "fn",
                       Name      => "format3",
                       Func      => Format3'Access);
      T.Context.Set_Function_Mapper (Fn);
      --  Create the expression
      E := Create_Expression ("#{fn:format1(10)} #{fn:format2(10,12)}", T.Context);
      Result := E.Get_Value (T.Context);
      Ada.Text_IO.Put_Line ("Result: " & EL.Objects.To_String (Result));
   end Test_Function_Evaluation;

   --  Test evaluation of method expression
   procedure Test_Method_Evaluation (T : in out Test) is
      use Action_Bean;

      A1 : constant Action_Access := new Action;
      A2 : constant Action_Access := new Action;
      P  : constant Person_Access := Create_Person ("Joe", "Black", 42);
      M  : EL.Expressions.Method_Expression :=
        Create_Expression (Context => T.Context,
                           Expr    => "#{action.notify}");
   begin
      T.Context.Set_Variable ("action", A1);
      Proc_Action.Execute (M, Person (P.all), T.Context);

      Assert (T, P.Last_Name = A1.Person.Last_Name, "Name was not set");

      P.Last_Name := To_Unbounded_String ("John");
      T.Context.Set_Variable ("action", A2);
      Proc_Action.Execute (M, Person (P.all), T.Context);

      Assert (T, "John" = A2.Person.Last_Name, "Name was not set");
   end Test_Method_Evaluation;

   --  Test evaluation of method expression
   procedure Test_Invalid_Method (T : in out Test) is
      use Action_Bean;

      A1 : constant Action_Access := new Action;
      A2 : constant Action_Access := new Action;
      P  : constant Person_Access := Create_Person ("Joe", "Black", 42);
      M2 : EL.Expressions.Method_Expression;
      M  : EL.Expressions.Method_Expression :=
        Create_Expression (Context => T.Context,
                           Expr    => "#{action2.bar}");
   begin
      --  Bean is not found
      begin
         Proc_Action.Execute (M, Person (P.all), T.Context);
         Assert (T, False, "The Invalid_Variable exception was not raised");

      exception
         when EL.Expressions.Invalid_Variable =>
            null;
      end;

      T.Context.Set_Variable ("action2", A1);
      begin
         Proc_Action.Execute (M, Person (P.all), T.Context);
         Assert (T, False, "The Invalid_Method exception was not raised");

      exception
         when EL.Expressions.Invalid_Method =>
            null;
      end;

      --  M2 is not initialized, this should raise Invalid_Expression
      begin
         Proc_Action.Execute (M2, Person (P.all), T.Context);
         Assert (T, False, "The Invalid_Method exception was not raised");

      exception
         when EL.Expressions.Invalid_Expression =>
            null;
      end;

      --  Create a method expression with an invalid expression
      begin
         M := Create_Expression ("#{12+13}", T.Context);
         Assert (T, False, "The Invalid_Expression exception was not raised");

      exception
         when EL.Expressions.Invalid_Expression =>
            null;
      end;
   end Test_Invalid_Method;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      --  Test_Bean verifies several methods.  Register several times
      --  to enumerate what is tested.
      Suite.Add_Test (Caller.Create ("Test EL.Beans.Get_Value (constant expressions)",
                                      Test_Simple_Evaluation'Access));
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
      Suite.Add_Test (Caller.Create ("Test EL.Expressions.Get_Method_Info",
                                     Test_Method_Evaluation'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Expressions.Get_Method_Info (Invalid_method)",
                                     Test_Invalid_Method'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Functions.Set_Function (and evaluation)",
                                     Test_Function_Evaluation'Access));
   end Add_Tests;

end EL.Expressions.Tests;

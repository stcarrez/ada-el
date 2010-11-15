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

with EL.Functions;
with EL.Functions.Default;
with EL.Expressions;
with Test_Bean;
with Action_Bean;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Util.Log.Loggers;
with Util.Tests;
package body EL.Expressions.Tests is

   use Test_Bean;
   use EL.Expressions;
   use AUnit.Test_Fixtures;
   use Ada.Strings.Unbounded;

   use Util.Log;
   use Util.Tests;

   LOG : constant Util.Log.Loggers.Logger := Loggers.Create ("Tests");

   procedure Free is new Ada.Unchecked_Deallocation (Object => EL.Contexts.Default.Default_Context'Class,
                                                     Name   => EL.Contexts.Default.Default_Context_Access);

   procedure Check_Error (T    : in out Test'Class;
                          Expr : in String);

   --  Set up performed before each test case
   procedure Set_Up (T : in out Test) is
   begin
--        Ada.Text_IO.Put_Line ("Allocate context");
      T.Context := new EL.Contexts.Default.Default_Context;
   end Set_Up;

   --  Tear down performed after each test case
   procedure Tear_Down (T : in out Test) is
   begin
--        Ada.Text_IO.Put_Line ("Free context");
      Free (T.Context);
   end Tear_Down;

   --  Check that evaluating an expression raises an exception
   procedure Check_Error (T    : in out Test'Class;
                          Expr : in String) is
      E : Expression;
   begin
      begin
         E := Create_Expression (Context => T.Context.all, Expr => Expr);

         T.Assert (Condition => False,
                   Message => "Evaluation of '" & Expr & "' should raise an exception");

      exception
         when Invalid_Expression =>
            T.Assert (Condition => E.Is_Constant,
                      Message   => "Invalid expression value");
      end;

   end Check_Error;

   --  ------------------------------
   --  Check that evaluating an expression returns the expected result
   --  (to keep the test simple, results are only checked using strings)
   --  ------------------------------
   procedure Check (T      : in out Test;
                    Expr   : in String;
                    Expect : in String) is
      E : constant Expression := Create_Expression (Context => T.Context.all,
                                                    Expr    => Expr);
      V : constant Object := E.Get_Value (Context => T.Context.all);
   begin
      T.Assert (Condition => To_String (V) = Expect,
                Message => "Evaluate '" & Expr & "' returned '" & To_String (V)
                & "' when we expect '" & Expect & "'");

      declare
         E2 : constant Expression := E.Reduce_Expression (Context => T.Context.all);
         V2 : constant Object := E2.Get_Value (Context => T.Context.all);
      begin
         T.Assert (To_String (V2) = Expect,
                   "Reduce produced incorrect result: " & To_String (V2));
      end;
   end Check;

   --  ------------------------------
   --  Test evaluation of expression using a bean
   --  ------------------------------
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
      Check (T, "#{(3 * 2) + (5 * 4) - (2*3)}", "20");
      Check (T, "#{3 <= 2}", "FALSE");
      Check (T, "#{3 < 2}", "FALSE");
      Check (T, "#{3 > 2}", "TRUE");
      Check (T, "#{3 >= 2}", "TRUE");
      Check (T, "#{3 >= 2 && 2 >= 3}", "FALSE");
      Check (T, "#{3 >= 4 || 2 < 3}", "TRUE");
      Check (T, "#{3 > 4 ? 1 : 2}", "2");
      Check (T, "#{3 < 4 ? 1 : 2}", "1");
      Check (T, "#{3 == 3}", "TRUE");
      Check (T, "#{3 != 3}", "FALSE");
      Check (T, "#{true and false}", "FALSE");
      Check (T, "#{true or false}", "TRUE");
      Check (T, "#{- 23}", "-23");
      Check (T, "#{1.0}", "1");
      Check (T, "#{'a\'b'}", "a'b");
      Check (T, "'a'", "'a'");
      Check (T, "\#{test}", "#{test}");
      Check (T, "\${test}", "${test}");
   end Test_Simple_Evaluation;

   --  ------------------------------
   --  Test evaluation of expression using a bean
   --  ------------------------------
   procedure Test_Bean_Evaluation (T : in out Test) is
      P : Person_Access := Create_Person ("Joe", "Black", 42);
   begin
      T.Context.all.Set_Variable ("user", P);

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
--      Check (T, "#{user.date}", To_String (To_Object (P.Date)));
      Check (T, "#{user.weight}", To_String (To_Object (P.Weight)));

      P.Age := P.Age + 1;
      Check (T, "#{user.age}", "43");
      Check (T, "#{user.firstName & ' ' & user.lastName}", "Joe Black");

      Check (T, "#{user.firstName eq 'Joe'}", "TRUE");
      Check (T, "#{user.firstName ne 'Joe'}", "FALSE");
      Check (T, "#{user.firstName eq 'Boe' or user.firstName eq 'Joe'}", "TRUE");
      Check (T, "Joe is #{user.age} year#{user.age > 0 ? 's' : ''} old",
             "Joe is 43 years old");

      Free (P);
   end Test_Bean_Evaluation;

   --  ------------------------------
   --  Test evaluation of expression using a bean
   --  ------------------------------
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
      Check_Error (T, "#{name");
      Check_Error (T, "#{name.name");
      Check_Error (T, "#{name.name.name");
      Check_Error (T, "#{name:");
      Check_Error (T, "#{name:name");
      Check_Error (T, "#{name:name(");
      Check_Error (T, "#{name:name(12");
      Check_Error (T, "#{name:name(12,");
      Check_Error (T, "#{name:name(12,+");
      Check_Error (T, "#{name:name(12,12");
      Check_Error (T, "#{name:name(12,12,");
      Check_Error (T, "#{name:name(12,12,+");
      Check_Error (T, "#{name:name(12,12,12");
      Check_Error (T, "#{name:name(12,12,12.12");
      Check_Error (T, "#{name:name(12,12,12.12,12");
      Check_Error (T, "#{,}");
      Check_Error (T, "#{.}");
      Check_Error (T, "#{:}");
      Check_Error (T, "#{~}");
      Check_Error (T, "#{@}");
   end Test_Parse_Error;

   --  ------------------------------
   --  Function to format a string
   --  ------------------------------
   function Format1 (Arg : EL.Objects.Object) return EL.Objects.Object is
      S : constant String := To_String (Arg);
   begin
      return To_Object ("[" & S & "]");
   end Format1;

   --  ------------------------------
   --  Function to format a string
   --  ------------------------------
   function Format2 (Arg1, Arg2 : EL.Objects.Object) return EL.Objects.Object is
      S1 : constant String := To_String (Arg1);
      S2 : constant String := To_String (Arg2);
      begin
      return To_Object ("[" & S1 & "-" & S2 & "]");
   end Format2;

   --  ------------------------------
   --  Function to format a string
   --  ------------------------------
   function Format3 (Arg1, Arg2, Arg3 : EL.Objects.Object) return EL.Objects.Object is
      S1 : constant String := To_String (Arg1);
      S2 : constant String := To_String (Arg2);
      S3 : constant String := To_String (Arg3);
   begin
      return To_Object ("[" & S1 & "-" & S2 & "-" & S3 & "]");
   end Format3;

   --  ------------------------------
   --  Function to format a string
   --  ------------------------------
   function Format4 (Arg1, Arg2, Arg3, Arg4 : EL.Objects.Object) return EL.Objects.Object is
      S1 : constant String := To_String (Arg1);
      S2 : constant String := To_String (Arg2);
      S3 : constant String := To_String (Arg3);
      S4 : constant String := To_String (Arg4);
   begin
      return To_Object ("[" & S1 & "-" & S2 & "-" & S3 & "-" & S4 & "]");
   end Format4;

   --  ------------------------------
   --  Test function evaluation
   --  ------------------------------
   procedure Test_Function_Evaluation (T : in out Test) is
      P  : Person_Access := Create_Person ("Joe", "Black", 42);
      Fn : aliased EL.Functions.Default.Default_Function_Mapper;
   begin
      T.Context.all.Set_Variable ("user", P);

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
      Fn.Set_Function (Namespace => "fx",
                       Name      => "format4",
                       Func      => Format4'Access);
      T.Context.Set_Function_Mapper (Fn'Unchecked_Access);

      --  Create the expression with function call and check the result
      Check (T, "#{fn:format1(10)} #{fn:format2(10,12)}", "[10] [10-12]");
      Check (T, "#{fn:format2(10,20)} #{fn:format3(20,32,33)}", "[10-20] [20-32-33]");
      Check (T, "#{fx:format4(10,20,30,40)}", "[10-20-30-40]");

      Check (T, "#{fn:format1(user.age)}", "[42]");
      Check (T, "#{fn:format2(user.age,10)}", "[42-10]");
      Check (T, "#{fn:format2(10,user.age)}", "[10-42]");
      Check (T, "#{fn:format3(user.age,user.age,user.age)}", "[42-42-42]");
      Check (T, "#{fn:format3(10,user.age,user.age)}", "[10-42-42]");
      Check (T, "#{fn:format3(10,10,user.age)}", "[10-10-42]");
      Check (T, "#{fx:format4(user.age,user.age,user.age,user.age)}", "[42-42-42-42]");
      Check (T, "#{fx:format4(user.age,10,user.age,user.age)}", "[42-10-42-42]");
      Check (T, "#{fx:format4(user.age,10,10,user.age)}", "[42-10-10-42]");
      Check (T, "#{fx:format4(user.age,10,10,10)}", "[42-10-10-10]");

      Check_Error (T, "#{fx:format4(12(");
      Check_Error (T, "#{fx:format4(12,12(");
      Check_Error (T, "#{fx:format4(12,12,12(");
      Check_Error (T, "#{fx:format4(12,12,12,12(");

      Free (P);
   end Test_Function_Evaluation;

   --  ------------------------------
   --  Test evaluation of method expression
   --  ------------------------------
   procedure Test_Method_Evaluation (T : in out Test) is
      use Action_Bean;

      A1 : Action_Access := new Action;
      A2 : Action_Access := new Action;
      P  : Person_Access := Create_Person ("Joe", "Black", 42);
      M  : EL.Expressions.Method_Expression :=
        Create_Expression (Context => T.Context.all,
                           Expr    => "#{action.notify}");
   begin
      T.Context.all.Set_Variable ("action", A1);
      A1.Count := 0;
      A2.Count := 0;
      Proc_Action.Execute (M, Person (P.all), T.Context.all);

      Assert (T, P.Last_Name = A1.Person.Last_Name, "Name was not set");

      P.Last_Name := To_Unbounded_String ("John");
      T.Context.all.Set_Variable ("action", A2);
      Proc_Action.Execute (M, Person (P.all), T.Context.all);

      Assert (T, "John" = A2.Person.Last_Name, "Name was not set");

      M := Create_Expression (Context => T.Context.all,
                              Expr    => "#{action.notify2}");

      --  Execute the method expression and check the count value.
      for I in 1 .. 5 loop
         Proc2_Action.Execute (M, Person (P.all), I, T.Context.all);

         Assert (T, I = A2.Count, "Count was not set");
         Assert (T, 0 = A1.Count, "First action modified as side effect");
      end loop;

      Free (P);
      Free (A1);
      Free (A2);
   end Test_Method_Evaluation;

   --  ------------------------------
   --  Test evaluation of invalid method expression
   --  ------------------------------
   procedure Test_Invalid_Method (T : in out Test) is
      use Action_Bean;

      A1 : Action_Access := new Action;
      A2 : Action_Access := new Action;
      P  : Person_Access := Create_Person ("Joe", "Black", 42);
      M2 : EL.Expressions.Method_Expression;
      M  : EL.Expressions.Method_Expression :=
        Create_Expression (Context => T.Context.all,
                           Expr    => "#{action2.bar}");
   begin
      --  Bean is not found
      begin
         Proc_Action.Execute (M, Person (P.all), T.Context.all);
         Assert (T, False, "The Invalid_Variable exception was not raised");

      exception
         when EL.Expressions.Invalid_Variable =>
            null;
      end;

      T.Context.all.Set_Variable ("action2", A1);
      begin
         Proc_Action.Execute (M, Person (P.all), T.Context.all);
         Assert (T, False, "The Invalid_Method exception was not raised");

      exception
         when EL.Expressions.Invalid_Method =>
            null;
      end;

      --  M2 is not initialized, this should raise Invalid_Expression
      begin
         Proc_Action.Execute (M2, Person (P.all), T.Context.all);
         Assert (T, False, "The Invalid_Method exception was not raised");

      exception
         when EL.Expressions.Invalid_Expression =>
            null;
      end;

      --  Create a method expression with an invalid expression
      begin
         M := Create_Expression ("#{12+13}", T.Context.all);
         Assert (T, False, "The Invalid_Expression exception was not raised");

      exception
         when EL.Expressions.Invalid_Expression =>
            null;
      end;

      Free (P);
      Free (A1);
      Free (A2);
   end Test_Invalid_Method;

   --  ------------------------------
   --  Test the use of a value expression.
   --  ------------------------------
   procedure Test_Value_Expression (T : in out Test) is
      P  : Person_Access := Create_Person ("Joe", "Black", 42);
      V1 : constant Expression := Create_Expression (Context => T.Context.all,
                                                     Expr => "#{user.age}");
   begin
      T.Context.all.Set_Variable ("user", P);
      for I in 1 .. 30 loop
         declare
            VE : constant Value_Expression := Create_Expression (V1);
         begin
            VE.Set_Value (Context => T.Context.all,
                          Value   => EL.Objects.To_Object (I));

            Assert_Equals (T, I, P.Age, "The value expression did not set the age");

            Assert (T, not VE.Is_Readonly (T.Context.all),
                    "Value expression should not be readonly");
         end;
      end loop;
      Free (P);
   end Test_Value_Expression;

   --  ------------------------------
   --  Test the invalid value expression
   --  ------------------------------
   procedure Test_Invalid_Value_Expression (T : in out Test) is
      V1 : constant Expression := Create_Expression (Context => T.Context.all,
                                                     Expr => "#{12 + 3}");
      V2 : constant Expression := Create_Expression (Context => T.Context.all,
                                                     Expr => "#{user.firstName}");
   begin
      --  Check that an exception is raised when the expression is not a value expression.
      begin
         declare
            VE : constant Value_Expression := Create_Expression (V1);
         begin
            Assert (T, False, "No exception raised for an invalid value expression");
            VE.Set_Value (Context => T.Context.all, Value => EL.Objects.Null_Object);
         end;
      exception
         when Invalid_Expression =>
            null;
      end;

      --  Check that an exception is raised when the bean is not found.
      declare
         VE : constant Value_Expression := Create_Expression (V2);
      begin
         VE.Set_Value (Context => T.Context.all,
                       Value   => EL.Objects.To_Object (Integer (2)));
         Assert (T, False, "No exception raised when setting the value expression");

      exception
         when Invalid_Variable =>
            null;
      end;
   end Test_Invalid_Value_Expression;

   --  ------------------------------
   --  Info about object sizes
   --  ------------------------------
   procedure Test_Object_Sizes (T : in out Test) is
      pragma Unreferenced (T);

      V    : EL.Objects.Object;
      Expr : EL.Expressions.Expression;
   begin
      LOG.Info ("EL.Objects.Object size = {0} bytes", Integer'Image (V'Size / 8));
      LOG.Info ("EL.Expression.Expression size = {0} bytes", Integer'Image (Expr'Size / 8));
   end Test_Object_Sizes;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      --  Test_Bean verifies several methods.  Register several times
      --  to enumerate what is tested.
      Suite.Add_Test (Caller.Create ("Object sizes",
                                      Test_Object_Sizes'Access));
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
      Suite.Add_Test (Caller.Create ("Test EL.Expressions.Set_Value",
                                      Test_Value_Expression'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Expressions.Set_Value (raise Invalid_Variable)",
                                      Test_Invalid_Value_Expression'Access));
   end Add_Tests;

end EL.Expressions.Tests;

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
with EL.Expressions;
with EL.Objects;
with EL.Objects.Enums;
with EL.Objects.Time;
with EL.Contexts;
with EL.Contexts.Default;
with Ada.Calendar;
with EL.Objects.Discrete_Tests;
with EL.Objects.Time.Tests;
with Ada.Calendar.Formatting;
with Ada.Calendar.Conversions;
with Interfaces.C;
with EL.Expressions.Tests;
package body EL.Testsuite is

   use Interfaces.C;
   use EL.Objects;
   use Ada.Calendar;
   use Ada.Calendar.Conversions;

   function "+" (Left, Right : Boolean) return Boolean;
   function "-" (Left, Right : Boolean) return Boolean;

   function "-" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time;
   function "+" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time;
   function Time_Value (S : String) return Ada.Calendar.Time;

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

   package Test_Integer is new
     EL.Objects.Discrete_Tests (Test_Type      => Integer,
                                To_Type        => EL.Objects.To_Integer,
                                To_Object_Test => EL.Objects.To_Object,
                                Value          => Integer'Value,
                                Test_Name      => "Integer",
                                Test_Values    => "-100,1,0,1,1000");

   package Test_Long_Integer is new
     EL.Objects.Discrete_Tests (Test_Type      => Long_Integer,
                                To_Type        => EL.Objects.To_Long_Integer,
                                To_Object_Test => EL.Objects.To_Object,
                                Value          => Long_Integer'Value,
                                Test_Name      => "Long_Integer",
                                Test_Values    => "-100,1,0,1,1000");

   package Test_Long_Long_Integer is new
     EL.Objects.Discrete_Tests (Test_Type      => Long_Long_Integer,
                                To_Type        => EL.Objects.To_Long_Long_Integer,
                                To_Object_Test => EL.Objects.To_Object,
                                Value          => Long_Long_Integer'Value,
                                Test_Name      => "Long_Long_Integer",
                                Test_Values => "-10000000000000,1,0,1,1000_000_000_000");

   function "-" (Left, Right : Boolean) return Boolean is
   begin
      return Left and Right;
   end "-";

   function "+" (Left, Right : Boolean) return Boolean is
   begin
      return Left or Right;
   end "+";

   package Test_Boolean is new
     EL.Objects.Discrete_Tests (Test_Type      => Boolean,
                                To_Type        => EL.Objects.To_Boolean,
                                To_Object_Test => EL.Objects.To_Object,
                                Value          => Boolean'Value,
                                Test_Name      => "Boolean",
                                Test_Values    => "false,true");

   type Color is (WHITE, BLACK, RED, GREEN, BLUE, YELLOW);

   package Color_Object is new EL.Objects.Enums (Color, ROUND_VALUE => True);

   function "-" (Left, Right : Color) return Color is
      N : constant Integer := Color'Pos (Left) - Color'Pos (Right);
   begin
      if N >= 0 then
         return Color'Val ((Color'Pos (WHITE) + N) mod 6);
      else
         return Color'Val ((Color'Pos (WHITE) - N) mod 6);
      end if;
   end "-";

   function "+" (Left, Right : Color) return Color is
      N : constant Integer := Color'Pos (Left) + Color'Pos (Right);
   begin
      return Color'Val ((Color'Pos (WHITE) + N) mod 6);
   end "+";

   package Test_Enum is new
     EL.Objects.Discrete_Tests (Test_Type      => Color,
                                To_Type        => Color_Object.To_Value,
                                To_Object_Test => Color_Object.To_Object,
                                Value          => Color'Value,
                                Test_Name      => "Color",
                                Test_Values    => "BLACK,RED,GREEN,BLUE,YELLOW");

   function Time_Value (S : String) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Value (S);
   end Time_Value;

   function "+" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time is
   begin
      return To_Ada_Time (To_Unix_Time (Left) + To_Unix_Time (Right));
   end "+";

   function "-" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time is
   begin
      return To_Ada_Time (To_Unix_Time (Left) - To_Unix_Time (Right));
   end "-";

   package Test_Time is new
     EL.Objects.Discrete_Tests (Test_Type      => Ada.Calendar.Time,
                                To_Type        => EL.Objects.Time.To_Time,
                                To_Object_Test => EL.Objects.Time.To_Object,
                                Value          => Time_Value,
                                Test_Name      => "Time",
                                Test_Values => "1970-03-04 12:12:00,1975-05-04 13:13:10");

   package Test_Float is new
     EL.Objects.Discrete_Tests (Test_Type      => Float,
                                To_Type        => EL.Objects.To_Float,
                                To_Object_Test => EL.Objects.To_Object,
                                Value          => Float'Value,
                                Test_Name      => "Float",
                                Test_Values    => "1.2,3.3,-3.3");

   package Test_Long_Float is new
     EL.Objects.Discrete_Tests (Test_Type      => Long_Float,
                                To_Type        => EL.Objects.To_Long_Float,
                                To_Object_Test => EL.Objects.To_Object,
                                Value          => Long_Float'Value,
                                Test_Name      => "Long_Float",
                                Test_Values    => "1.2,3.3,-3.3");

   package Test_Long_Long_Float is new
     EL.Objects.Discrete_Tests (Test_Type      => Long_Long_Float,
                                To_Type        => EL.Objects.To_Long_Long_Float,
                                To_Object_Test => EL.Objects.To_Object,
                                Value          => Long_Long_Float'Value,
                                Test_Name      => "Long_Long_Float",
                                Test_Values    => "1.2,3.3,-3.3");

   package Caller is new AUnit.Test_Caller (Test);

   Tests : aliased Test_Suite;

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := Tests'Access;
   begin
      Ret.Add_Test (Caller.Create ("Test To_Object (Integer)",
                                   Test_To_Object_Integer'Access));
      Ret.Add_Test (Caller.Create ("Test Expressions", Test_Expression'Access));
      Test_Boolean.Add_Tests (Ret);
      Test_Integer.Add_Tests (Ret);
      Test_Long_Integer.Add_Tests (Ret);
      Test_Long_Long_Integer.Add_Tests (Ret);
      Test_Time.Add_Tests (Ret);
      Test_Float.Add_Tests (Ret);
      Test_Long_Float.Add_Tests (Ret);
      Test_Long_Long_Float.Add_Tests (Ret);
      Test_Enum.Add_Tests (Ret);
      EL.Objects.Time.Tests.Add_Tests (Ret);
      EL.Expressions.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end EL.Testsuite;

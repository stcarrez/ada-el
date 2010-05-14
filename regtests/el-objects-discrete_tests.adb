-----------------------------------------------------------------------
--  el-objects-tests - Generic simple test for discrete object types
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
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Calendar;
package body EL.Objects.Discrete_Tests is

   use EL.Objects;
   use AUnit.Assertions;
   use Ada.Strings.Fixed;

   procedure Test_Eq (V : String; N : Test_Type);
   procedure Test_Conversion (V : String; N : Test_Type);
   procedure Test_Lt_Gt (V : String; N : Test_Type);
   procedure Test_Sub (V : String; N : Test_Type);
   procedure Test_Add (V : String; N : Test_Type);

   --  Generic test for To_Object and To_XXX types
   --  Several values are specified in the Test_Values string.
   generic
      with procedure Basic_Test (V : String; N : Test_Type);
   procedure Test_Basic_Object (T : in out Test);

   procedure Test_Basic_Object (T : in out Test) is
      pragma Unreferenced (T);

      Pos, Next : Natural;
   begin
      Pos := Test_Values'First;
      while Pos <= Test_Values'Last loop
         Next := Index (Test_Values, ",", Pos);
         if Next < Pos then
            Next := Test_Values'Last + 1;
         end if;
         declare
            V : constant String := Test_Values (Pos .. Next - 1);
            N : constant Test_Type := Value (V);
         begin
            Basic_Test (V, N);
         end;
         Pos := Next + 1;
      end loop;
   end Test_Basic_Object;

   --  ------------------------------
   --  Test EL.Objects.To_Object
   --  ------------------------------
   procedure Test_Conversion (V : String; N : Test_Type) is
      Value : EL.Objects.Object;
   begin
      Value := To_Object (V);
      Assert (Condition => To_Type (Value) = N,
              Message   => Test_Name & " returned invalid value: "
              & To_String (Value) & " when we expected: " & V);

      Assert (Condition => V = To_String (Value),
              Message   => Test_Name & ".To_String returned invalid value: "
                & To_String (Value) & " when we expected: " & V);
   end Test_Conversion;
   procedure Test_To_Object is new Test_Basic_Object (Basic_Test => Test_Conversion);

   --  ------------------------------
   --  Test EL.Objects."+"
   --  ------------------------------
   procedure Test_Add (V : String; N : Test_Type) is
      Value : EL.Objects.Object := To_Object_Test (N);
   begin
      Value := Value + To_Object_Test (N);
      Assert (Condition => To_Type (Value) = N + N,
              Message   => Test_Name & " returned invalid value: "
              & To_String (Value) & " when we expected: " & V);
   end Test_Add;

   procedure Test_Add is new Test_Basic_Object (Test_Add);

   --  ------------------------------
   --  Test EL.Objects."-"
   --  ------------------------------
   procedure Test_Sub (V : String; N : Test_Type) is
      pragma Unreferenced (V);

      Value : EL.Objects.Object;
   begin
      Value := To_Object_Test (N) - To_Object_Test (N);
      Assert (Condition => To_Type (Value) = N - N,
              Message   => Test_Name & " returned invalid value: "
              & To_String (Value) & " when we expected: 0");
   end Test_Sub;

   procedure Test_Sub is new Test_Basic_Object (Test_Sub);

   --  ------------------------------
   --  Test EL.Objects."<" and EL.Objects.">"
   --  ------------------------------
   procedure Test_Lt_Gt (V : String; N : Test_Type) is
      Res   : Boolean;
      Is_Neg : constant Boolean := Index (V, "-") > 0;
   begin
      Res := To_Object_Test (N) < To_Object_Test (N);
      Assert (Condition => Res = False,
              Message   => Test_Name & ".'<' returned invalid value: "
              & Boolean'Image (Res) & " when we expected: false");
      Res := To_Object_Test (N) > To_Object_Test (N);
      Assert (Condition => Res = False,
              Message   => Test_Name & ".'>' returned invalid value: "
              & Boolean'Image (Res) & " when we expected: false");
      Res := To_Object_Test (N) + To_Object_Test (N) < To_Object_Test (N);
      Assert (Condition => Res = Is_Neg,
              Message   => Test_Name & ".'<' returned invalid value: "
              & Boolean'Image (Res) & " when we expected: "
              & Boolean'Image (Is_Neg)
              & " with value: " & V);
      Res := To_Object_Test (N) > To_Object_Test (N) + To_Object_Test (N);
      Assert (Condition => Res = Is_Neg,
              Message   => Test_Name & ".'>' returned invalid value: "
              & Boolean'Image (Res) & " when we expected: "
              & Boolean'Image (Is_Neg)
              & " with value: " & V);
      if V /= "0" and V /= "false" and V /= "true" then
         Res := To_Object_Test (N) < To_Object_Test (N) + To_Object_Test (N);
         Assert (Condition => Res = not Is_Neg,
                 Message   => Test_Name & ".'<' returned invalid value: "
                 & Boolean'Image (Res) & " when we expected: "
                 & Boolean'Image (not Is_Neg)
                 & " with value: " & V);
         Res := To_Object_Test (N) + To_Object_Test (N) > To_Object_Test (N);
         Assert (Condition => Res = not Is_Neg,
                 Message   => Test_Name & ".'>' returned invalid value: "
                 & Boolean'Image (Res) & " when we expected: "
                 & Boolean'Image (not Is_Neg)
                 & " with value: " & V);
      end if;
   end Test_Lt_Gt;

   procedure Test_Lt_Gt is new Test_Basic_Object (Test_Lt_Gt);

   --  ------------------------------
   --  Test EL.Objects."="
   --  ------------------------------
   procedure Test_Eq (V : String; N : Test_Type) is
      Res   : Boolean;
   begin
      Res := To_Object_Test (N) = To_Object_Test (N);
      Assert (Condition => Res,
              Message   => Test_Name & ".'=' returned invalid value: "
              & Boolean'Image (Res) & " when we expected: true");

      Res := To_Object_Test (N) = To_Object ("Something" & V);
      Assert (Condition => Res = False,
              Message   => Test_Name & ".'=' returned invalid value: "
                & Boolean'Image (Res) & " where we expected: False");
   end Test_Eq;
   procedure Test_Eq is new Test_Basic_Object (Test_Eq);

   --  ------------------------------
   --  Test EL.Objects."="
   --  ------------------------------
   procedure Test_Perf (V : String; N : Test_Type) is
      use Ada.Calendar;

      Res   : Boolean;
      Start : Ada.Calendar.Time;
      Value : EL.Objects.Object := To_Object_Test (N);
      D     : Duration;
   begin
      Start := Ada.Calendar.Clock;
      for I in 1 .. 1_000 loop
         declare
            V : EL.Objects.Object := Value;
         begin
            V := V + V;
         end;
      end loop;
      D := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line ("Perf " & Test_Name & ": " & Duration'Image (D * 1000.0));
   end Test_Perf;
   procedure Test_Perf is new Test_Basic_Object (Test_Perf);

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test EL.Objects.To_Object." & Test_Name,
                                     Test_To_Object'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Objects.To_String." & Test_Name,
                                     Test_To_Object'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Objects.'='." & Test_Name,
                                     Test_Eq'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Objects.'+'." & Test_Name,
                                     Test_Add'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Objects.'-'." & Test_Name,
                                     Test_Sub'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Objects.'<'." & Test_Name,
                                     Test_Lt_Gt'Access));
      Suite.Add_Test (Caller.Create ("Test EL.Objects.'>'." & Test_Name,
                                     Test_Lt_Gt'Access));
      Suite.Add_Test (Caller.Create ("Performance EL.Objects.'>'." & Test_Name,
        Test_Perf'Access));
   end Add_Tests;

end EL.Objects.Discrete_Tests;

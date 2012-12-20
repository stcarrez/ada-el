-----------------------------------------------------------------------
--  EL.Objects.Time.Tests - Testsuite time objects
--  Copyright (C) 2010 Stephane Carrez
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
with Ada.Calendar.Formatting;
with Util.Log.Loggers;
with Util.Tests;
package body EL.Objects.Time.Tests is

   use Ada.Calendar;
   use Util.Log;
   use Util.Tests;

   LOG : constant Util.Log.Loggers.Logger := Loggers.Create ("Tests");

   --  ------------------------------
   --  Test evaluation of expression using a bean
   --  ------------------------------
   procedure Test_Time_Object (T : in out Test) is
      C : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      V : constant EL.Objects.Object := To_Object (C);
   begin
      T.Assert (Is_Null (V) = False, "Object holding a time value must not be null");
      T.Assert (Is_Empty (V) = False, "Object holding a time value must not be empty");
      T.Assert (Get_Type (V) = TYPE_TIME, "Object holding a time value must be TYPE_TIME");

      T.Assert (C = To_Time (V), "Invalid time returned by To_Time");
   end Test_Time_Object;

   --  ------------------------------
   --  Test time to string conversion
   --  ------------------------------
   procedure Test_Time_To_String (T : in out Test) is
      C  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      V  : constant EL.Objects.Object := To_Object (C);
      S  : constant EL.Objects.Object := Cast_String (V);
      V2 : constant EL.Objects.Object := Cast_Time (S);
   begin
      --  Both 3 values should be the same.
      LOG.Info ("Time S : {0}", To_String (S));
      LOG.Info ("Time V : {0}", To_String (V));
      LOG.Info ("Time V2: {0}", To_String (V2));

      Assert_Equals (T, To_String (S), To_String (V), "Invalid time conversion (V)");
      Assert_Equals (T, To_String (S), To_String (V2), "Invalid time conversion (V2)");

      --  The Cast_String looses accuracy so V and V2 may not be equal.
      T.Assert (V >= V2, "Invalid time to string conversion");

      --  Check the time value taking into account the 1 sec accuracy that was lost.
      T.Assert (C >= To_Time (V2), "Invalid time returned by To_Time (T > expected)");
      T.Assert (C < To_Time (V2) + 1.0, "Invalid time returned by To_Time (T + 1 < expected)");
   end Test_Time_To_String;

   --  ------------------------------
   --  Test time add and subtract
   --  ------------------------------
   procedure Test_Time_Add (T : in out Test) is
      C  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      V  : constant EL.Objects.Object := To_Object (C);
      Dt : constant EL.Objects.Object := To_Object (Integer (10));
      V2 : constant EL.Objects.Object := V + Dt;
      V3 : constant EL.Objects.Object := V2 - Dt;
   begin
      T.Assert (V3 = V, "Adding and substracting 10 seconds should result in the same time");
      T.Assert (V2 > V, "Invalid comparison for time");
      T.Assert (V3 < V2, "Invalid comparison for time");
   end Test_Time_Add;

   package Caller is new Util.Test_Caller (Test, "EL.Objects.Time");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      --  Test_Bean verifies several methods.  Register several times
      --  to enumerate what is tested.
      Caller.Add_Test (Suite, "Test EL.Objects.Time.To_Object - Is_Null, Is_Empty, Get_Type",
                       Test_Time_Object'Access);
      Caller.Add_Test (Suite, "Test EL.Objects.Time.To_Object - To_Time",
                       Test_Time_Object'Access);
      Caller.Add_Test (Suite, "Test EL.Objects.Time.To_String - Cast_Time",
                       Test_Time_To_String'Access);
      Caller.Add_Test (Suite, "Test EL.Objects.Time.To_String - Cast_Time",
                       Test_Time_Add'Access);
   end Add_Tests;

end EL.Objects.Time.Tests;

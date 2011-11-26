-----------------------------------------------------------------------
--  EL.Beans.Tests - Testsuite for EL.Beans
--  Copyright (C) 2011 Stephane Carrez
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

with Test_Bean;
with Util.Beans.Objects;
with EL.Contexts.Default;
package body EL.Beans.Tests is

   use Util.Tests;
   use Util.Beans.Objects;
   use Test_Bean;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test EL.Beans.Add_Parameter",
                       Test_Add_Parameter'Access);
      Caller.Add_Test (Suite, "Test EL.Beans.Initialize",
                       Test_Initialize'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test Add_Parameter
   --  ------------------------------
   procedure Test_Add_Parameter (T : in out Test) is
      P       : Param_Vectors.Vector;
      Context : EL.Contexts.Default.Default_Context;
   begin
      --  Add a constant parameter.
      Add_Parameter (Container => P,
                     Name      => "firstName",
                     Value     => "my name",
                     Context   => Context);
      Assert_Equals (T, 1, Integer (P.Length), "Parameter was not added");
      T.Assert_Equals ("firstName", P.Element (1).Name, "Invalid parameter name");
      T.Assert (P.Element (1).Value.Is_Constant, "Value should be a constant");
      T.Assert_Equals ("my name",
                       Util.Beans.Objects.To_String (P.Element (1).Value.Get_Value (Context)),
                       "Invalid value");

      --  Add an expression parameter.
      Add_Parameter (Container => P,
                     Name      => "lastName",
                     Value     => "#{name}",
                     Context   => Context);
      Assert_Equals (T, 2, Integer (P.Length), "Parameter was not added");
   end Test_Add_Parameter;

   --  ------------------------------
   --  Test the Initialize procedure with a set of expressions
   --  ------------------------------
   procedure Test_Initialize (T : in out Test) is
      P       : Param_Vectors.Vector;
      Context : EL.Contexts.Default.Default_Context;
      User    : Person_Access := Create_Person ("Joe", "Black", 42);
      Bean    : Person_Access := Create_Person ("", "", 0);
   begin
      Context.Set_Variable ("user", User);
      Add_Parameter (P, "firstName", "#{user.firstName}", Context);
      Add_Parameter (P, "lastName", "#{user.lastName}", Context);
      Add_Parameter (P, "age", "#{user.age + 2}", Context);

      Initialize (Bean.all, P, Context);
      T.Assert_Equals ("Joe", Bean.First_Name, "First name not initialized");
      T.Assert_Equals ("Black", Bean.Last_Name, "Last name not initialized");
      Assert_Equals (T, 44, Integer (Bean.Age), "Age was not initialized");
      Free (Bean);
      Free (User);
   end Test_Initialize;

end EL.Beans.Tests;

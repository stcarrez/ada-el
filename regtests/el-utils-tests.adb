-----------------------------------------------------------------------
--  el-contexts-tests - Tests the EL contexts
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with Util.Properties;
with Util.Beans.Objects;

with EL.Contexts.Default;
package body EL.Utils.Tests is

   use Util.Tests;
   use Util.Beans.Objects;

   package Caller is new Util.Test_Caller (Test, "EL.Utils");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test EL.Utils.Expand",
                       Test_Expand_Properties'Access);
      Caller.Add_Test (Suite, "Test EL.Utils.Expand (recursion)",
                       Test_Expand_Recursion'Access);
      Caller.Add_Test (Suite, "Test EL.Utils.Eval",
                       Test_Eval'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test expand list of properties
   --  ------------------------------
   procedure Test_Expand_Properties (T : in out Test) is
      Context       : EL.Contexts.Default.Default_Context;
      Props         : Util.Properties.Manager;
      Result        : Util.Properties.Manager;
   begin
      Props.Set ("context", "#{homedir}/context");
      Props.Set ("homedir", "#{home}/#{user}");
      Props.Set ("unknown", "#{not_defined}");
      Props.Set ("user", "joe");
      Props.Set ("home", "/home");
      EL.Utils.Expand (Source  => Props,
                       Into    => Result,
                       Context => Context);

      Assert_Equals (T, "joe", String '(Result.Get ("user")), "Invalid expansion");
      Assert_Equals (T, "/home/joe", String '(Result.Get ("homedir")), "Invalid expansion");
      Assert_Equals (T, "/home/joe/context", String '(Result.Get ("context")),
                     "Invalid expansion");
      Assert_Equals (T, "", String '(Result.Get ("unknown")), "Invalid expansion");
   end Test_Expand_Properties;

   --  ------------------------------
   --  Test expand list of properties
   --  ------------------------------
   procedure Test_Expand_Recursion (T : in out Test) is
      Context       : EL.Contexts.Default.Default_Context;
      Props         : Util.Properties.Manager;
      Result        : Util.Properties.Manager;
   begin
      Props.Set ("context", "#{homedir}/context");
      Props.Set ("homedir", "#{home}/#{user}");
      Props.Set ("user", "joe");
      Props.Set ("home", "#{context}");
      EL.Utils.Expand (Source  => Props,
                       Into    => Result,
                       Context => Context);

      Assert_Equals (T, "joe", String '(Result.Get ("user")), "Invalid expansion");
      Assert_Equals (T, "/joe/context/joe/context/joe/context/joe",
                     String '(Result.Get ("homedir")),
                     "Invalid expansion");
   end Test_Expand_Recursion;

   --  ------------------------------
   --  Test expand list of properties
   --  ------------------------------
   procedure Test_Eval (T : in out Test) is
      Context : EL.Contexts.Default.Default_Context;
   begin
      Assert_Equals (T, "1", EL.Utils.Eval (Value   => "1",
                                           Context => Context), "Invalid eval <empty string>");
      Assert_Equals (T, "3", EL.Utils.Eval (Value   => "#{2+1}",
                                            Context => Context), "Invalid eval <valid expr>");
      declare
         Value, Result : Util.Beans.Objects.Object;

      begin
         Value := Util.Beans.Objects.To_Object (Integer '(123));
         Result := EL.Utils.Eval (Value  => Value,
                                  Context => Context);
         T.Assert (Value = Result, "Eval should return the same object");

         Value := Util.Beans.Objects.To_Object (String '("345"));
         Result := EL.Utils.Eval (Value  => Value,
                                  Context => Context);
         T.Assert (Value = Result, "Eval should return the same object");
      end;
   end Test_Eval;

end EL.Utils.Tests;

-----------------------------------------------------------------------
--  el-contexts-tests - Tests the EL contexts
--  Copyright (C) 2011, 2015 Stephane Carrez
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

with EL.Expressions;
with EL.Contexts.Properties;
with EL.Contexts.Default;
with EL.Contexts.TLS;
package body EL.Contexts.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "EL.Contexts");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test EL.Contexts.Properties.Get_Value",
                       Test_Context_Properties'Access);
      Caller.Add_Test (Suite, "Test EL.Contexts.TLS.Current",
                       Test_Context_TLS'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test Add_Parameter
   --  ------------------------------
   procedure Test_Context_Properties (T : in out Test) is

      function Eval (Item : in String) return String;

      Context       : EL.Contexts.Default.Default_Context;
      Prop_Resolver : aliased EL.Contexts.Properties.Property_Resolver;
      Props         : Util.Properties.Manager;

      function Eval (Item : in String) return String is
         Expr   : constant Expressions.Expression
           := EL.Expressions.Create_Expression (Item, Context);
         Result : constant Util.Beans.Objects.Object := Expr.Get_Value (Context);
      begin
         return Util.Beans.Objects.To_String (Result);
      end Eval;

   begin
      Props.Set ("user", "joe");
      Props.Set ("home", "/home/joe");
      Prop_Resolver.Set_Properties (Props);

      Context.Set_Resolver (Prop_Resolver'Unchecked_Access);

      Assert_Equals (T, "joe", Eval ("#{user}"), "Invalid evaluation of #{user}");
      Assert_Equals (T, "/home/joe", Eval ("#{home}"), "Invalid evaluation of #{home}");
   end Test_Context_Properties;

   --  ------------------------------
   --  Test the thread local EL context.
   --  ------------------------------
   procedure Test_Context_TLS (T : in out Test) is
   begin
      T.Assert (EL.Contexts.TLS.Current = null, "The TLS expression context must be null");
      declare
         C1 : EL.Contexts.TLS.TLS_Context;
         T1 : EL.Contexts.ELContext_Access;
      begin
         T1 := EL.Contexts.TLS.Current;
         T.Assert (T1 /= null, "The TLS expression context must not be null");
         T.Assert (C1.Get_Variable_Mapper = T1.Get_Variable_Mapper,
                   "The TLS expression context is not valid");
         declare
            C2 : EL.Contexts.TLS.TLS_Context;
            T2 : EL.Contexts.ELContext_Access;
         begin
            T2 := EL.Contexts.TLS.Current;
            T.Assert (T2 /= null, "The TLS expression context must not be null");
            T.Assert (C2.Get_Variable_Mapper = T2.Get_Variable_Mapper,
                      "The TLS expression context is not valid");
            T.Assert (T1 /= T2, "The TLS expression context must be changed");
         end;
         T.Assert (T1 = EL.Contexts.TLS.Current, "The TLS expression context must be restored");
      end;
      T.Assert (EL.Contexts.TLS.Current = null, "The TLS expression context must be null");
   end Test_Context_TLS;

end EL.Contexts.Tests;

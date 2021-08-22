-----------------------------------------------------------------------
--  el-methods-proc_2 -- Procedure Binding with 2 arguments
--  Copyright (C) 2010, 2011, 2012, 2020, 2021 Stephane Carrez
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
with Util.Beans.Objects;

package body EL.Methods.Proc_2 is

   use EL.Expressions;

   --  ------------------------------
   --  Returns True if the method is a valid method which accepts the arguments
   --  defined by the package instantiation.
   --  ------------------------------
   function Is_Valid (Method : in EL.Expressions.Method_Info) return Boolean is
   begin
      if Method.Binding = null then
         return False;
      else
         return Method.Binding.all in Binding'Class;
      end if;
   end Is_Valid;

   --  ------------------------------
   --  Execute the method describe by the method expression
   --  and with the given context.  The method signature is:
   --
   --   procedure F (Obj    : in out <Bean>;
   --                Param1 : in Param1_Type;
   --                Param2 : in Param2_Type);
   --
   --  where <Bean> inherits from <b>Readonly_Bean</b>
   --  (See <b>Bind</b> package)
   --
   --  Raises <b>Invalid_Method</b> if the method referenced by
   --  the method expression does not exist or does not match
   --  the signature.
   --  ------------------------------
   procedure Execute (Method  : in EL.Expressions.Method_Expression'Class;
                      Param1  : in Param1_Type;
                      Param2  : in Param2_Type;
                      Context : in EL.Contexts.ELContext'Class) is
      Info   : constant Method_Info := Method.Get_Method_Info (Context);
   begin
      if Info.Binding = null then
         raise EL.Expressions.Invalid_Method with "Method not found";
      end if;

      --  If the binding has the wrong type, we are trying to invoke
      --  a method with a different signature.
      if not (Info.Binding.all in Binding'Class) then
         raise EL.Expressions.Invalid_Method
         with "Invalid signature for method '" & Info.Binding.Name.all & "'";
      end if;
      declare
         Proxy  : constant Binding_Access := Binding (Info.Binding.all)'Access;
      begin
         Proxy.Method (Util.Beans.Objects.To_Bean (Info.Object), Param1, Param2);
      end;
   end Execute;

   --  ------------------------------
   --  Proxy for the binding.
   --  The proxy declares the binding definition that links
   --  the name to the function and it implements the necessary
   --  object conversion to translate the <b>Readonly_Bean</b>
   --  object to the target object type.
   --  ------------------------------
   package body Bind is
      procedure Method_Access (O  : access Util.Beans.Basic.Readonly_Bean'Class;
                               P1 : Param1_Type;
                               P2 : Param2_Type) is
         Object : constant access Bean := Bean (O.all)'Access;
      begin
         Method (Object.all, P1, P2);
      end Method_Access;
   end Bind;

end EL.Methods.Proc_2;

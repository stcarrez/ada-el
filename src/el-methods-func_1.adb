-----------------------------------------------------------------------
--  EL.Methods.Func_1 -- Function Bindings with 1 argument
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

package body EL.Methods.Func_1 is

   use EL.Expressions;

   --  ------------------------------
   --  Execute the method describe by the method expression
   --  and with the given context.  The method signature is:
   --
   --   function F (Obj : <Bean>; Param : Param1_Type) return Return_Type;
   --
   --  where <Bean> inherits from <b>Readonly_Bean</b>
   --  (See <b>Bind</b> package)
   --
   --  Raises <b>Invalid_Method</b> if the method referenced by
   --  the method expression does not exist or does not match
   --  the signature.
   --  ------------------------------
   function Execute (Method  : in EL.Expressions.Method_Expression'Class;
                     Param   : in Param1_Type;
                     Context : in EL.Contexts.ELContext'Class) return Return_Type is
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
	 return Proxy.Method (Info.Object.all, Param);
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
      function Method_Access (O  : Util.Beans.Basic.Readonly_Bean'Class;
			      P1 : Param1_Type) return Return_Type is
	 Object : constant Bean        := Bean (O);
	 Result : constant Return_Type := Method (Object, P1);
      begin
	 return Result;
      end Method_Access;
   end Bind;

end EL.Methods.Func_1;

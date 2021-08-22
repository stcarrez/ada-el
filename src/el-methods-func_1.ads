-----------------------------------------------------------------------
--  el-methods-func_1 -- Function Bindings with 1 argument
--  Copyright (C) 2010, 2011, 2012, 2013, 2021 Stephane Carrez
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

with EL.Expressions;
with EL.Contexts;
with Util.Beans.Methods;
with Util.Beans.Basic;
generic
   type Param1_Type (<>) is limited private;
   type Return_Type (<>) is private;
package EL.Methods.Func_1 is

   use Util.Beans.Methods;

   --  Returns True if the method is a valid method which accepts the arguments
   --  defined by the package instantiation.
   function Is_Valid (Method : in EL.Expressions.Method_Info) return Boolean;

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
   function Execute (Method  : in EL.Expressions.Method_Expression'Class;
                     Param   : in Param1_Type;
                     Context : in EL.Contexts.ELContext'Class) return Return_Type;

   --  Function access to the proxy.
   type Proxy_Access is
     access function (O : in Util.Beans.Basic.Readonly_Bean'Class;
                      P : in Param1_Type) return Return_Type;

   --  The binding record which links the method name
   --  to the proxy function.
   type Binding is new Method_Binding with record
      Method : Proxy_Access;
   end record;
   type Binding_Access is access constant Binding;

   --  Proxy for the binding.
   --  The proxy declares the binding definition that links
   --  the name to the function and it implements the necessary
   --  object conversion to translate the <b>Readonly_Bean</b>
   --  object to the target object type.
   generic
      --  Name of the method (as exposed in the EL expression)
      Name : String;

      --  The bean type
      type Bean is new Util.Beans.Basic.Readonly_Bean with private;

      --  The bean method to invoke
      with function Method (O  : in Bean;
                            P1 : in Param1_Type) return Return_Type;
   package Bind is

      --  Method that <b>Execute</b> will invoke.
      function Method_Access (O  : in Util.Beans.Basic.Readonly_Bean'Class;
                              P1 : in Param1_Type) return Return_Type;

      F_NAME : aliased constant String := Name;

      --  The proxy binding that can be exposed through
      --  the <b>Method_Bean</b> interface.
      Proxy  : aliased constant Binding
        := Binding '(Name => F_NAME'Access,
                     Method => Method_Access'Access);
   end Bind;

end EL.Methods.Func_1;

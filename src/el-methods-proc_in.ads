-----------------------------------------------------------------------
--  el-methods-proc_in -- Procedure Binding with 1 in argument
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with EL.Expressions;
with EL.Contexts;
with Util.Beans.Methods;
with Util.Beans.Basic;
generic
   type Param1_Type (<>) is limited private;
package EL.Methods.Proc_In is

   use Util.Beans.Methods;

   --  Returns True if the method is a valid method which accepts the arguments
   --  defined by the package instantiation.
   function Is_Valid (Method : in EL.Expressions.Method_Info) return Boolean;

   --  Execute the method describe by the method expression
   --  and with the given context.  The method signature is:
   --
   --   procedure F (Obj   : in out <Bean>;
   --                Param : in out Param1_Type);
   --
   --  where <Bean> inherits from <b>Readonly_Bean</b>
   --  (See <b>Bind</b> package)
   --
   --  Raises <b>Invalid_Method</b> if the method referenced by
   --  the method expression does not exist or does not match
   --  the signature.
   procedure Execute (Method  : in EL.Expressions.Method_Expression'Class;
                      Param   : in Param1_Type;
                      Context : in EL.Contexts.ELContext'Class);

   --  Execute the method describe by the method binding object.
   --  The method signature is:
   --
   --   procedure F (Obj   : in out <Bean>;
   --                Param : in out Param1_Type);
   --
   --  where <Bean> inherits from <b>Readonly_Bean</b>
   --  (See <b>Bind</b> package)
   --
   --  Raises <b>Invalid_Method</b> if the method referenced by
   --  the method expression does not exist or does not match
   --  the signature.
   procedure Execute (Method  : in EL.Expressions.Method_Info;
                      Param   : in Param1_Type);

   --  Function access to the proxy.
   type Proxy_Access is
      access procedure (O : access Util.Beans.Basic.Readonly_Bean'Class;
                        P : in Param1_Type);

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
      type Bean is abstract limited new Util.Beans.Basic.Readonly_Bean with private;

      --  The bean method to invoke
      with procedure Method (O  : in out Bean;
                             P1 : in Param1_Type);
   package Bind is

      --  Method that <b>Execute</b> will invoke.
      procedure Method_Access (O  : access Util.Beans.Basic.Readonly_Bean'Class;
                               P1 : in Param1_Type);

      F_NAME : aliased constant String := Name;

      --  The proxy binding that can be exposed through
      --  the <b>Method_Bean</b> interface.
      Proxy  : aliased constant Binding
        := Binding '(Name => F_NAME'Access,
                     Method => Method_Access'Access);
   end Bind;

end EL.Methods.Proc_In;

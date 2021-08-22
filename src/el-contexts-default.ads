-----------------------------------------------------------------------
--  el-contexts-default -- Default contexts for evaluating an expression
--  Copyright (C) 2009, 2010, 2011, 2015, 2021 Stephane Carrez
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
with EL.Variables;
with Ada.Finalization;

private with Util.Beans.Objects.Maps;
--  private with EL.Objects.Maps;  creates Assert_Failure sem_ch10.adb:2691
package EL.Contexts.Default is

   --  ------------------------------
   --  Default Context
   --  ------------------------------
   --  Context information for expression evaluation.
   type Default_Context is new Ada.Finalization.Controlled and ELContext with private;
   type Default_Context_Access is access all Default_Context'Class;

   overriding
   procedure Finalize (Obj : in out Default_Context);

   --  Retrieves the ELResolver associated with this ELcontext.
   overriding
   function Get_Resolver (Context : Default_Context) return ELResolver_Access;

   --  Retrieves the VariableMapper associated with this ELContext.
   overriding
   function Get_Variable_Mapper (Context : Default_Context)
                                 return access EL.Variables.Variable_Mapper'Class;

   --  Retrieves the FunctionMapper associated with this ELContext.
   --  The FunctionMapper is only used when parsing an expression.
   overriding
   function Get_Function_Mapper (Context : Default_Context)
                                 return EL.Functions.Function_Mapper_Access;

   --  Set the function mapper to be used when parsing an expression.
   overriding
   procedure Set_Function_Mapper (Context : in out Default_Context;
                                  Mapper  : access EL.Functions.Function_Mapper'Class);

   --  Set the VariableMapper associated with this ELContext.
   overriding
   procedure Set_Variable_Mapper (Context : in out Default_Context;
                                  Mapper  : access EL.Variables.Variable_Mapper'Class);

   --  Set the ELResolver associated with this ELcontext.
   procedure Set_Resolver (Context  : in out Default_Context;
                           Resolver : in ELResolver_Access);

   procedure Set_Variable (Context : in out Default_Context;
                           Name    : in String;
                           Value   : access Util.Beans.Basic.Readonly_Bean'Class);

   --  Handle the exception during expression evaluation.
   overriding
   procedure Handle_Exception (Context : in Default_Context;
                               Ex      : in Ada.Exceptions.Exception_Occurrence);

   --  ------------------------------
   --  Guarded Context
   --  ------------------------------
   --  The <b>Guarded_Context</b> is a proxy context that allows to handle exceptions
   --  raised when evaluating expressions.  The <b>Handler</b> procedure will be called
   --  when an exception is raised when the expression is evaluated.  This allows to
   --  report an error message and ignore or not the exception (See ASF).
   type Guarded_Context (Handler : not null access
                           procedure (Ex : in Ada.Exceptions.Exception_Occurrence);
                         Context : ELContext_Access)
     is new Ada.Finalization.Limited_Controlled and ELContext with null record;
   type Guarded_Context_Access is access all Default_Context'Class;

   --  Retrieves the ELResolver associated with this ELcontext.
   overriding
   function Get_Resolver (Context : in Guarded_Context) return ELResolver_Access;

   --  Retrieves the Variable_Mapper associated with this ELContext.
   overriding
   function Get_Variable_Mapper (Context : in Guarded_Context)
                                 return access EL.Variables.Variable_Mapper'Class;

   --  Retrieves the Function_Mapper associated with this ELContext.
   --  The Function_Mapper is only used when parsing an expression.
   overriding
   function Get_Function_Mapper (Context : in Guarded_Context)
                                 return EL.Functions.Function_Mapper_Access;

   --  Set the function mapper to be used when parsing an expression.
   overriding
   procedure Set_Function_Mapper (Context : in out Guarded_Context;
                                  Mapper  : access EL.Functions.Function_Mapper'Class);

   --  Set the Variable_Mapper associated with this ELContext.
   overriding
   procedure Set_Variable_Mapper (Context : in out Guarded_Context;
                                  Mapper  : access EL.Variables.Variable_Mapper'Class);

   --  Handle the exception during expression evaluation.
   overriding
   procedure Handle_Exception (Context : in Guarded_Context;
                               Ex      : in Ada.Exceptions.Exception_Occurrence);

   --  ------------------------------
   --  Default Resolver
   --  ------------------------------
   type Default_ELResolver is new ELResolver with private;
   type Default_ELResolver_Access is access all Default_ELResolver'Class;

   --  Get the value associated with a base object and a given property.
   overriding
   function Get_Value (Resolver : Default_ELResolver;
                       Context  : ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : Unbounded_String) return EL.Objects.Object;

   --  Set the value associated with a base object and a given property.
   overriding
   procedure Set_Value (Resolver : in out Default_ELResolver;
                        Context  : in ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in EL.Objects.Object);

   --  Register the value under the given name.
   procedure Register (Resolver : in out Default_ELResolver;
                       Name     : in Unbounded_String;
                       Value    : in Util.Beans.Basic.Readonly_Bean_Access);

   --  Register the value under the given name.
   procedure Register (Resolver : in out Default_ELResolver;
                       Name     : in Unbounded_String;
                       Value    : in EL.Objects.Object);

private

   type Default_Context is new Ada.Finalization.Controlled and ELContext with record
      Var_Mapper         : EL.Variables.Variable_Mapper_Access;
      Resolver           : ELResolver_Access;
      Function_Mapper    : EL.Functions.Function_Mapper_Access;
      Var_Mapper_Created : Boolean := False;
   end record;

   type Default_ELResolver is new ELResolver with record
      Map : EL.Objects.Maps.Map;
   end record;

end EL.Contexts.Default;

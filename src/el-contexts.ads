-----------------------------------------------------------------------
--  EL.Contexts -- Contexts for evaluating an expression
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

--  The expression context provides information to resolve runtime
--  information when evaluating an expression.  The context provides
--  a resolver whose role is to find variables given their name.

with EL.Objects;
with EL.Beans;
with Ada.Strings.Unbounded;
with EL.Functions;
limited with EL.Variables;
package EL.Contexts is

   pragma Preelaborate;

   use EL.Objects;
   use Ada.Strings.Unbounded;

   type ELContext;

   --  ------------------------------
   --  Expression Resolver
   --  ------------------------------
   --  Enables customization of variable and property resolution
   --  behavior for EL expression evaluation.
   type ELResolver is interface;
   type ELResolver_Access is access all ELResolver'Class;

   --  Get the value associated with a base object and a given property.
   function Get_Value (Resolver : ELResolver;
                       Context  : ELContext'Class;
                       Base     : access EL.Beans.Readonly_Bean'Class;
                       Name     : Unbounded_String) return Object is abstract;

   --  Set the value associated with a base object and a given property.
   procedure Set_Value (Resolver : in ELResolver;
                        Context  : in ELContext'Class;
                        Base     : access EL.Beans.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in Object) is abstract;

   --  ------------------------------
   --  Expression Context
   --  ------------------------------
   --  Context information for expression evaluation.
   type ELContext is interface;
   type ELContext_Access is access all ELContext'Class;

   --  Retrieves the ELResolver associated with this ELcontext.
   function Get_Resolver (Context : ELContext) return ELResolver_Access is abstract;

   --  Retrieves the VariableMapper associated with this ELContext.
   function Get_Variable_Mapper (Context : ELContext)
                            return access EL.Variables.VariableMapper'Class is abstract;

   --  Set the variable mapper associated with this ELContext.
   procedure Set_Variable_Mapper (Context : in out ELContext;
                                  Mapper  : access EL.Variables.VariableMapper'Class)
     is abstract;

   --  Retrieves the FunctionMapper associated with this ELContext.
   --  The FunctionMapper is only used when parsing an expression.
   function Get_Function_Mapper (Context : ELContext)
                                 return EL.Functions.Function_Mapper_Access is abstract;

   --  Set the function mapper associated with this ELContext.
   procedure Set_Function_Mapper (Context : in out ELContext;
                                  Mapper  : access EL.Functions.Function_Mapper'Class)
     is abstract;

end EL.Contexts;

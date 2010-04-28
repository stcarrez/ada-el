-----------------------------------------------------------------------
--  EL.Contexts -- Default contexts for evaluating an expression
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

with EL.Variables.Default;
package body EL.Contexts.Default is

   --  ------------------------------
   --  Retrieves the ELResolver associated with this ELcontext.
   --  ------------------------------
   overriding
   function Get_Resolver (Context : Default_Context) return ELResolver_Access is
   begin
      return Context.Resolver;
   end Get_Resolver;

   --  ------------------------------
   --  Retrieves the VariableMapper associated with this ELContext.
   --  ------------------------------
   overriding
   function Get_Variable_Mapper (Context : Default_Context)
                                 return access EL.Variables.VariableMapper'Class is
   begin
      return Context.Var_Mapper;
   end Get_Variable_Mapper;

   --  ------------------------------
   --  Retrieves the FunctionMapper associated with this ELContext.
   --  The FunctionMapper is only used when parsing an expression.
   --  ------------------------------
   overriding
   function Get_Function_Mapper (Context : Default_Context)
                                 return EL.Functions.Function_Mapper_Access is
   begin
      return Context.Function_Mapper;
   end Get_Function_Mapper;

   procedure Set_Variable (Context : in out Default_Context;
                           Name    : in String;
                           Value   : access EL.Beans.Readonly_Bean'Class) is
   begin
      if Context.Var_Mapper = null then
         Context.Var_Mapper := new EL.Variables.Default.Default_Variable_Mapper;
      end if;
      Context.Var_Mapper.Bind (Name, Value);
   end Set_Variable;

   --  ------------------------------
   --  Get the value associated with a base object and a given property.
   --  ------------------------------
   overriding
   function Get_Value (Resolver : Default_ELResolver;
                       Context  : ELContext'Class;
                       Base     : access EL.Beans.Readonly_Bean'Class;
                       Name     : Unbounded_String) return Object is
      pragma Unreferenced (Resolver);
      pragma Unreferenced (Context);

      R : Object;
   begin
      if Base /= null then
         return Base.Get_Value (To_String (Name));
      end if;

      return R;
   end Get_Value;

   --  ------------------------------
   --  Set the value associated with a base object and a given property.
   --  ------------------------------
   overriding
   procedure Set_Value (Resolver : in Default_ELResolver;
                        Context  : in ELContext'Class;
                        Base     : access EL.Beans.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in Object) is
   begin
      null;
   end Set_Value;

end EL.Contexts.Default;

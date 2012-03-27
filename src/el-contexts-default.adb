-----------------------------------------------------------------------
--  EL.Contexts -- Default contexts for evaluating an expression
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with EL.Variables.Default;
package body EL.Contexts.Default is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => EL.Variables.Variable_Mapper'Class,
                                     Name   => EL.Variables.Variable_Mapper_Access);

   --  ------------------------------
   --  Retrieves the ELResolver associated with this ELcontext.
   --  ------------------------------
   overriding
   function Get_Resolver (Context : Default_Context) return ELResolver_Access is
   begin
      return Context.Resolver;
   end Get_Resolver;

   --  ------------------------------
   --  Set the ELResolver associated with this ELcontext.
   --  ------------------------------
   procedure Set_Resolver (Context  : in out Default_Context;
                           Resolver : in ELResolver_Access) is
   begin
      Context.Resolver := Resolver;
   end Set_Resolver;

   --  ------------------------------
   --  Retrieves the Variable_Mapper associated with this ELContext.
   --  ------------------------------
   overriding
   function Get_Variable_Mapper (Context : Default_Context)
                                 return access EL.Variables.Variable_Mapper'Class is
   begin
      return Context.Var_Mapper;
   end Get_Variable_Mapper;

   --  ------------------------------
   --  Retrieves the Function_Mapper associated with this ELContext.
   --  The Function_Mapper is only used when parsing an expression.
   --  ------------------------------
   overriding
   function Get_Function_Mapper (Context : Default_Context)
                                 return EL.Functions.Function_Mapper_Access is
   begin
      return Context.Function_Mapper;
   end Get_Function_Mapper;

   --  ------------------------------
   --  Set the function mapper to be used when parsing an expression.
   --  ------------------------------
   overriding
   procedure Set_Function_Mapper (Context : in out Default_Context;
                                  Mapper  : access EL.Functions.Function_Mapper'Class) is
   begin
      if Mapper = null then
         Context.Function_Mapper := null;
      else
         Context.Function_Mapper := Mapper.all'Unchecked_Access;
      end if;
   end Set_Function_Mapper;

   --  ------------------------------
   --  Set the Variable_Mapper associated with this ELContext.
   --  ------------------------------
   overriding
   procedure Set_Variable_Mapper (Context : in out Default_Context;
                                  Mapper  : access EL.Variables.Variable_Mapper'Class) is
      use EL.Variables;
   begin
      if Context.Var_Mapper_Created then
         Free (Context.Var_Mapper);
      end if;
      if Mapper = null then
         Context.Var_Mapper := null;
      else
         Context.Var_Mapper := Mapper.all'Unchecked_Access;
      end if;
   end Set_Variable_Mapper;

   procedure Set_Variable (Context : in out Default_Context;
                           Name    : in String;
                           Value   : access Util.Beans.Basic.Readonly_Bean'Class) is
      use EL.Variables;
   begin
      if Context.Var_Mapper = null then
         Context.Var_Mapper := new EL.Variables.Default.Default_Variable_Mapper;
         Context.Var_Mapper_Created := True;
      end if;
      Context.Var_Mapper.Bind (Name, EL.Objects.To_Object (Value, EL.Objects.STATIC));
   end Set_Variable;

   --  Handle the exception during expression evaluation.
   overriding
   procedure Handle_Exception (Context : in Default_Context;
                               Ex      : in Ada.Exceptions.Exception_Occurrence) is
   begin
      null;
   end Handle_Exception;

   --  ------------------------------
   --  Guarded Context
   --  ------------------------------

   --  ------------------------------
   --  Retrieves the ELResolver associated with this ELcontext.
   --  ------------------------------
   overriding
   function Get_Resolver (Context : in Guarded_Context) return ELResolver_Access is
   begin
      return Context.Context.Get_Resolver;
   end Get_Resolver;

   --  ------------------------------
   --  Retrieves the Variable_Mapper associated with this ELContext.
   --  ------------------------------
   overriding
   function Get_Variable_Mapper (Context : in Guarded_Context)
                                 return access EL.Variables.Variable_Mapper'Class is
   begin
      return Context.Context.Get_Variable_Mapper;
   end Get_Variable_Mapper;

   --  ------------------------------
   --  Retrieves the Function_Mapper associated with this ELContext.
   --  The Function_Mapper is only used when parsing an expression.
   --  ------------------------------
   overriding
   function Get_Function_Mapper (Context : in Guarded_Context)
                                 return EL.Functions.Function_Mapper_Access is
   begin
      return Context.Context.Get_Function_Mapper;
   end Get_Function_Mapper;

   --  ------------------------------
   --  Set the function mapper to be used when parsing an expression.
   --  ------------------------------
   overriding
   procedure Set_Function_Mapper (Context : in out Guarded_Context;
                                  Mapper  : access EL.Functions.Function_Mapper'Class) is
   begin
      Context.Context.Set_Function_Mapper (Mapper);
   end Set_Function_Mapper;

   --  ------------------------------
   --  Set the Variable_Mapper associated with this ELContext.
   --  ------------------------------
   overriding
   procedure Set_Variable_Mapper (Context : in out Guarded_Context;
                                  Mapper  : access EL.Variables.Variable_Mapper'Class) is
   begin
      Context.Context.Set_Variable_Mapper (Mapper);
   end Set_Variable_Mapper;

   --  ------------------------------
   --  Handle the exception during expression evaluation.
   --  ------------------------------
   overriding
   procedure Handle_Exception (Context : in Guarded_Context;
                               Ex      : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Context.Handler.all (Ex);
   end Handle_Exception;

   --  ------------------------------
   --  Get the value associated with a base object and a given property.
   --  ------------------------------
   overriding
   function Get_Value (Resolver : Default_ELResolver;
                       Context  : ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : Unbounded_String) return Object is
      pragma Unreferenced (Context);

      Key : constant String := To_String (Name);
   begin
      if Base /= null then
         return Base.Get_Value (Key);
      end if;

      declare
         Pos : constant Objects.Maps.Cursor := Resolver.Map.Find (Key);
      begin
         if Objects.Maps.Has_Element (Pos) then
            return Objects.Maps.Element (Pos);
         end if;
      end;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Set the value associated with a base object and a given property.
   --  ------------------------------
   overriding
   procedure Set_Value (Resolver : in out Default_ELResolver;
                        Context  : in ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in Object) is
   begin
      null;
   end Set_Value;

   --  ------------------------------
   --  Register the value under the given name.
   --  ------------------------------
   procedure Register (Resolver : in out Default_ELResolver;
                       Name     : in Unbounded_String;
                       Value    : access Util.Beans.Basic.Readonly_Bean'Class) is
   begin
      Resolver.Register (Name, To_Object (Value));
   end Register;

   --  ------------------------------
   --  Register the value under the given name.
   --  ------------------------------
   procedure Register (Resolver : in out Default_ELResolver;
                       Name     : in Unbounded_String;
                       Value    : in EL.Objects.Object) is
      Key : constant String := To_String (Name);
   begin
      Objects.Maps.Include (Resolver.Map, Key, Value);
   end Register;


   overriding
   procedure Finalize (Obj : in out Default_Context) is
   begin
      if Obj.Var_Mapper_Created then
         Free (Obj.Var_Mapper);
      end if;
   end Finalize;

end EL.Contexts.Default;

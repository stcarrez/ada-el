-----------------------------------------------------------------------
--  el-variables-default -- Default Variable Mapper
--  Copyright (C) 2009, 2010, 2011, 2012, 2021 Stephane Carrez
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
package body EL.Variables.Default is

   overriding
   procedure Bind (Mapper : in out Default_Variable_Mapper;
                   Name   : in String;
                   Value  : in EL.Objects.Object) is
      Expr : constant EL.Expressions.Value_Expression
        := EL.Expressions.Create_ValueExpression (Value);
   begin
      Mapper.Map.Include (Key      => To_Unbounded_String (Name),
                          New_Item => EL.Expressions.Expression (Expr));
   end Bind;

   overriding
   function Get_Variable (Mapper : Default_Variable_Mapper;
                          Name   : Unbounded_String)
                          return EL.Expressions.Expression is
      C : constant Variable_Maps.Cursor := Mapper.Map.Find (Name);
   begin
      if not Variable_Maps.Has_Element (C) then
         if Mapper.Next_Mapper /= null then
            return Mapper.Next_Mapper.all.Get_Variable (Name);
         end if;
         --  Avoid raising an exception if we can't resolve a variable.
         --  Instead, return a null expression.  This speeds up the resolution and
         --  creation of Ada bean in ASF framework (cost of exception is high compared to this).
         return E : EL.Expressions.Expression;
      end if;
      return Variable_Maps.Element (C);
   end Get_Variable;

   overriding
   procedure Set_Variable (Mapper : in out Default_Variable_Mapper;
                           Name   : in Unbounded_String;
                           Value  : in EL.Expressions.Expression) is
   begin
      Mapper.Map.Include (Key      => Name,
                          New_Item => Value);
   end Set_Variable;

   --  ------------------------------
   --  Set the next variable mapper that will be used to resolve a variable if
   --  the current variable mapper does not find a variable.
   --  ------------------------------
   procedure Set_Next_Variable_Mapper (Mapper      : in out Default_Variable_Mapper;
                                       Next_Mapper : in Variable_Mapper_Access) is
   begin
      Mapper.Next_Mapper := Next_Mapper;
   end Set_Next_Variable_Mapper;

end EL.Variables.Default;

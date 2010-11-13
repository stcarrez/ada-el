-----------------------------------------------------------------------
--  EL.Variables -- Variable mapper
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

--
--  The expression context provides information to resolve runtime
--  information when evaluating an expression.  The context provides
--  a resolver whose role is to find variables given their name.

with Ada.Strings.Unbounded;
with EL.Expressions;
with EL.Objects;
package EL.Variables is

   use Ada.Strings.Unbounded;

   No_Variable : exception;

   type VariableMapper is interface;
   type VariableMapper_Access is access all VariableMapper'Class;

   procedure Bind (Mapper : in out VariableMapper;
                   Name   : in String;
                   Value  : in EL.Objects.Object) is abstract;

   function Get_Variable (Mapper : VariableMapper;
                          Name   : Unbounded_String)
                          return EL.Expressions.Value_Expression is abstract;

   procedure Set_Variable (Mapper : in out VariableMapper;
                           Name   : in Unbounded_String;
                           Value  : in EL.Expressions.Value_Expression) is abstract;

end EL.Variables;

-----------------------------------------------------------------------
--  el-variables -- Variable mapper
--  Copyright (C) 2009, 2010, 2011, 2021, 2022 Stephane Carrez
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

   pragma Preelaborate;

   use Ada.Strings.Unbounded;

   No_Variable : exception;

   type Variable_Mapper is interface;
   type Variable_Mapper_Access is access all Variable_Mapper'Class;

   procedure Bind (Mapper : in out Variable_Mapper;
                   Name   : in String;
                   Value  : in EL.Objects.Object) is abstract;

   function Get_Variable (Mapper : Variable_Mapper;
                          Name   : Unbounded_String)
                          return EL.Expressions.Expression is abstract;

   procedure Set_Variable (Mapper : in out Variable_Mapper;
                           Name   : in Unbounded_String;
                           Value  : in EL.Expressions.Expression) is abstract;

end EL.Variables;

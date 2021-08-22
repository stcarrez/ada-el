-----------------------------------------------------------------------
--  el-variables-default -- Default Variable Mapper
--  Copyright (C) 2009, 2010, 2011, 2018, 2021 Stephane Carrez
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

private with Ada.Strings.Unbounded;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;
package EL.Variables.Default is

   --  Default Variable Mapper
   type Default_Variable_Mapper is new Variable_Mapper with private;

   overriding
   procedure Bind (Mapper : in out Default_Variable_Mapper;
                   Name   : in String;
                   Value  : in EL.Objects.Object);

   overriding
   function Get_Variable (Mapper : Default_Variable_Mapper;
                          Name   : Unbounded_String)
                          return EL.Expressions.Expression;

   overriding
   procedure Set_Variable (Mapper : in out Default_Variable_Mapper;
                           Name   : in Unbounded_String;
                           Value  : in EL.Expressions.Expression);

   --  Set the next variable mapper that will be used to resolve a variable if
   --  the current variable mapper does not find a variable.
   procedure Set_Next_Variable_Mapper (Mapper      : in out Default_Variable_Mapper;
                                       Next_Mapper : in Variable_Mapper_Access);

private

   use type EL.Expressions.Expression;

   package Variable_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type      => Unbounded_String,
                                 Element_Type  => EL.Expressions.Expression,
                                 Hash          => Ada.Strings.Unbounded.Hash,
                                 Equivalent_Keys => "=");

   type Default_Variable_Mapper is new Variable_Mapper with record
      Next_Mapper : Variable_Mapper_Access := null;
      Map         : Variable_Maps.Map;
   end record;

end EL.Variables.Default;

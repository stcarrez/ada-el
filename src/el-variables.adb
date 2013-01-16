-----------------------------------------------------------------------
--  EL.Variables -- Variable mapper
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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

package body EL.Variables is

   --  ------------------------------
   --  Get the Value_Expression that corresponds to the given variable name.
   --  ------------------------------
   function Get_Variable (Mapper : in Variable_Mapper'Class;
                          Name   : in Unbounded_String)
                          return EL.Expressions.Value_Expression is
      VE : constant EL.Expressions.Expression := Mapper.Get_Variable (Name);
   begin
      return EL.Expressions.Create_Expression (VE);
   end Get_Variable;

   --  ------------------------------
   --  Set the variable to the given value expression.
   --  ------------------------------
   procedure Set_Variable (Mapper : in out Variable_Mapper'Class;
                           Name   : in Unbounded_String;
                           Value  : in EL.Expressions.Value_Expression) is
   begin
      Mapper.Set_Variable (Name, EL.Expressions.Expression (Value));
   end Set_Variable;

end EL.Variables;

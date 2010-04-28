-----------------------------------------------------------------------
--  EL.Functions -- Default function mapper
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

--  The default function mapper allows to register
--  The expression context provides information to resolve runtime
--  information when evaluating an expression.  The context provides
--  a resolver whose role is to find variables given their name.

private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;
package EL.Functions.Default is

   --  ------------------------------
   --  Default Function mapper
   --  ------------------------------
   --
   type Default_Function_Mapper is new Function_Mapper with private;

   --  Find the function knowing its name.
   function Get_Function (Mapper : Default_Function_Mapper;
                          Name   : String) return Function_Access;

   --  Bind a name to a function.
   procedure Set_Function (Mapper : in out Default_Function_Mapper;
                           Name   : in String;
                           Func   : in Function_Access);

private

   use Ada.Strings.Unbounded;

   package Function_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type => Unbounded_String,
                                            Element_Type => Function_Access,
                                            Hash => Ada.Strings.Unbounded.Hash,
                                            Equivalent_Keys => "=");

   type Default_Function_Mapper is new Function_Mapper with record
      Map : Function_Maps.Map;
   end record;

end EL.Functions.Default;

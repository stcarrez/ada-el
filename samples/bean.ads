-----------------------------------------------------------------------
--  bean - A simple bean example
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
with EL.Objects;
with EL.Beans;
private with Ada.Strings.Unbounded;

package Bean is

   type Person is new EL.Beans.Bean with private;
   type Person_Access is access all Person'Class;

   function Create_Person (First_Name, Last_Name : String;
                           Age : Natural) return Person_Access;

   --  Get the value identified by the name.
   function Get_Value (From : Person; Name : String) return EL.Objects.Object;

   --  Set the value identified by the name.
   procedure Set_Value (From  : in out Person;
                        Name  : in String;
                        Value : in EL.Objects.Object);

   --  Function to format a string
   function Format (Arg : EL.Objects.Object) return EL.Objects.Object;

private

   use Ada.Strings.Unbounded;

   type Person is new EL.Beans.Bean with record
      Last_Name  : Unbounded_String;
      First_Name : Unbounded_String;
      Age        : Natural;
   end record;

end Bean;

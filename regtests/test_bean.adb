-----------------------------------------------------------------------
--  Test_Bean - A simple bean for unit tests
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
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

package body Test_Bean is

   use EL.Objects;

   FIRST_NAME : constant String := "firstName";
   LAST_NAME  : constant String := "lastName";
   AGE        : constant String := "age";
   WEIGHT     : constant String := "weight";

   function Create_Person (First_Name, Last_Name : String;
                           Age : Natural) return Person_Access is
   begin
      return new Person '(First_Name => To_Unbounded_String (First_Name),
                          Last_Name  => To_Unbounded_String (Last_Name),
                          Age        => Age,
                          Date       => Ada.Calendar.Clock,
                          Weight     => 12.0);
   end Create_Person;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : Person; Name : String) return EL.Objects.Object is
   begin
      if Name = FIRST_NAME then
         return To_Object (From.First_Name);
      elsif Name = LAST_NAME then
         return To_Object (From.Last_Name);
      elsif Name = AGE then
         return To_Object (From.Age);
      elsif Name = WEIGHT then
         return To_Object (From.Weight);
--        elsif Name = DATE then
--           return To_Object (From.Date);
      else
         return EL.Objects.Null_Object;
      end if;
   end Get_Value;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Person;
                        Name  : in String;
                        Value : in EL.Objects.Object) is
   begin
      if Name = FIRST_NAME then
         From.First_Name := To_Unbounded_String (Value);
      elsif Name = LAST_NAME then
         From.Last_Name := To_Unbounded_String (Value);
      elsif Name = AGE then
         From.Age := Natural (To_Integer (Value));
      elsif Name = WEIGHT then
         From.Weight := To_Long_Long_Float (Value);
--        elsif Name = DATE then
--           From.Date := To_Time (Value);
      else
         raise EL.Objects.No_Value;
      end if;
   end Set_Value;

   --  Function to format a string
   function Format (Arg : EL.Objects.Object) return EL.Objects.Object is
      S : constant String := To_String (Arg);
   begin
      return To_Object ("[" & S & "]");
   end Format;

end Test_Bean;

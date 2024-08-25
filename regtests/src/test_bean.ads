-----------------------------------------------------------------------
--  Test_Bean - A simple bean ffor unit tests
--  Copyright (C) 2009, 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Objects;
with Util.Beans.Basic;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;
package Test_Bean is

   use Ada.Strings.Unbounded;

   type Person is new Util.Beans.Basic.Bean with record
      Last_Name  : Unbounded_String;
      First_Name : Unbounded_String;
      Age        : Natural;
      Date       : Ada.Calendar.Time;
      Weight     : Long_Long_Float;
   end record;
   type Person_Access is access all Person'Class;

   function Create_Person (First_Name, Last_Name : String;
                           Age : Natural) return Person_Access;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : Person; Name : String) return EL.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Person;
                        Name  : in String;
                        Value : in EL.Objects.Object);

   --  Function to format a string
   function Format (Arg : EL.Objects.Object) return EL.Objects.Object;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Person'Class,
                                                     Name   => Person_Access);

end Test_Bean;

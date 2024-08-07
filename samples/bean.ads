-----------------------------------------------------------------------
--  bean - A simple bean example
--  Copyright (C) 2009, 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;
with Ada.Strings.Unbounded;
package Bean is

   use Ada.Strings.Unbounded;

   type Person is new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with private;
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

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Person)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --
   function Save (P : in Person; Name : in Unbounded_String) return Unbounded_String;

   function Print (P : in Person; Title : in String) return String;

   function Compute (B : Util.Beans.Basic.Bean'Class;
                     P1 : EL.Objects.Object) return EL.Objects.Object;

   --  Function to format a string
   function Format (Arg : EL.Objects.Object) return EL.Objects.Object;

   procedure Free (Object : in out Person_Access);
private

   type Person is new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with record
      Last_Name  : Unbounded_String;
      First_Name : Unbounded_String;
      Age        : Natural;
   end record;

end Bean;

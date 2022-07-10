-----------------------------------------------------------------------
--  bean - A simple bean example
--  Copyright (C) 2009, 2010, 2022 Stephane Carrez
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
with Util.Beans.Factory;
with EL.Methods.Func_String;
with EL.Methods.Func_Unbounded;
with Ada.Unchecked_Deallocation;
package body Bean is

   use EL.Objects;
   use EL.Methods;

   FIRST_NAME : constant String := "firstName";
   LAST_NAME  : constant String := "lastName";
   AGE        : constant String := "age";

   Null_Object : Object;

   function Create_Person (First_Name, Last_Name : String;
                           Age : Natural) return Person_Access is
   begin
      return new Person '(First_Name => To_Unbounded_String (First_Name),
                          Last_Name => To_Unbounded_String (Last_Name),
                          Age => Age);
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
      else
         return Null_Object;
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
      end if;
   end Set_Value;

   --
   function Save (P : in Person; Name : in Unbounded_String) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Result := P.Last_Name & Name;
      return Result;
   end Save;

   function Compute (B  : Util.Beans.Basic.Bean'Class;
                     P1 : EL.Objects.Object) return EL.Objects.Object is
      pragma Unreferenced (B);
   begin
      return P1;
   end Compute;

   --  Function to format a string
   function Format (Arg : EL.Objects.Object) return EL.Objects.Object is
      S : constant String := To_String (Arg);
   begin
      return To_Object ("[" & S & "]");
   end Format;

   function Print (P : in Person; Title : in String) return String is
   begin
      return Title & " ["
        & "Name=" & To_String (P.First_Name) & ", "
        & "Last_name=" & To_String (P.Last_Name) & "]";
   end Print;

   package Save_Binding is
     new Func_Unbounded.Bind (Bean        => Person,
                              Method      => Save,
                              Name        => "save");

   package Print_Binding is
     new Func_String.Bind (Bean        => Person,
                           Method      => Print,
                           Name        => "print");

   type Bean_Definition is new Util.Beans.Factory.Bean_Definition with null record;

   --  Create a bean.
   overriding
   function Create (Def : in Bean_Definition)
                    return Util.Beans.Basic.Readonly_Bean_Access;

   --  Free the bean instance.
   overriding
   procedure Destroy (Def  : in Bean_Definition;
                      Bean : in out Util.Beans.Basic.Readonly_Bean_Access);

   --  Create a bean.
   overriding
   function Create (Def : in Bean_Definition)
                    return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Person_Access := new Person;
   begin
      return Result.all'Access;
   end Create;

   --  Free the bean instance.
   overriding
   procedure Destroy (Def  : in Bean_Definition;
                      Bean : in out Util.Beans.Basic.Readonly_Bean_Access) is
   begin
      null;
   end Destroy;

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Save_Binding.Proxy'Access, Print_Binding.Proxy'Access);

   overriding
   function Get_Method_Bindings (From : in Person)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

   procedure Free (Object : in out Person_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => Person'Class,
                                                        Name   => Person_Access);

   begin
      Free (Object);
   end Free;

end Bean;

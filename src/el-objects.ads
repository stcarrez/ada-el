-----------------------------------------------------------------------
--  EL.Objects -- Generic Typed Data Representation
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

--  Provides a data type to manage entities of different types using the
--  same abstraction.
--
--  An ''Object' can hold one of the following values:
--   o a boolean
--   o a long long integer
--   o a date
--   o a string
--   o a wide wide string
--   o a generic data
--
--
--  Value : Object := To_Object ("something");
--  Value := Value + To_Object ("12");
--
with System;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
limited with El.Beans;
package EL.Objects is

   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Wide_Unbounded;

   --  Exception raised when an object cannot be converted to a given type.
   Conversion_Error : exception;

   --  ------------------------------
   --  Generic Object holding a value
   --  ------------------------------
   --  The object has a type represented by 'Object_Type'.
   --  It can hold any value while being tightly coupled with a type.
   --  The object can be converted to standard Ada types.
   type Object is private;

   --  The null object.
   Null_Object : constant Object;

   type Data_Type is (TYPE_NULL,
                      --  The object holds a boolean value.
                      TYPE_BOOLEAN,
                      --  The object holds an integer value (64-bits).
                      TYPE_INTEGER,
                      --  The object holds a floating point value.
                      TYPE_FLOAT,
                      --  The object holds a string
                      TYPE_STRING,
                      --  The object holds a wide wide string
                      TYPE_WIDE_STRING,
                      --  The object holds a date and time
                      TYPE_TIME,
                      --  The object holds a generic bean
                      TYPE_BEAN,
                      --  Something else, unknown
                      TYPE_EXTERNAL);


   --  ------------------------------
   --  Type definition
   --  ------------------------------
   --  The Object_Type describes a type.  It serves as a basis
   --  for type conversion.
   type Object_Type is interface;
   type Object_Type_Access is access constant Object_Type'Class;

   --  Get the type name
   function Get_Name (Type_Def : Object_Type) return String is abstract;

   --  Translate the object
   function To_String (Type_Def : Object_Type;
                       Value    : in Object) return String is abstract;

--   generic
--      with type T is (<>);
--   function To_Enum (Value : in Object) return T;

   --  ------------------------------
   --  Generic Object holding a value
   --  ------------------------------

   --  Get a type identification for the object value.
   function Get_Type (Value : in Object) return Data_Type;

   --  Get the type definition of the object value.
   function Get_Type (Value : in Object) return Object_Type'Class;

   --  Get the type name of this object.
   function Get_Type_Name (Value : Object) return String;

   --  Convert the object to the corresponding type.
   function To_String (Value : in Object) return String;
   function To_Wide_Wide_String (Value : in Object) return Wide_Wide_String;
   function To_Unbounded_String (Value : in Object) return Unbounded_String;
   function To_Unbounded_Wide_Wide_String (Value : in Object) return Unbounded_Wide_Wide_String;
   function To_Integer (Value : in Object) return Integer;
   function To_Boolean (Value : in Object) return Boolean;
   function To_Long_Integer (Value : in Object) return Long_Integer;
   function To_Long_Long_Integer (Value : in Object) return Long_Long_Integer;
   function To_Float (Value : in Object) return Float;
   function To_Long_Float (Value : in Object) return Long_Float;
   function To_Long_Long_Float (Value : in Object) return Long_Long_Float;
   function To_Time (Value : in Object) return Ada.Calendar.Time;

   function To_Bean (Value : in Object) return access EL.Beans.Readonly_Bean'Class;

   --  Convert the object to an object of another time.
   --  Force the object to be an integer.
   function Cast_Integer (Value : Object) return Object;

   --  Force the object to be a float.
   function Cast_Float (Value : Object) return Object;

   --  Force the object to be a time.
   function Cast_Time (Value : Object) return Object;

   --  Force the object to be a string.
   function Cast_String (Value : Object) return Object;

   --  Convert a value to a generic typed object.
   function To_Object (Value : in Integer) return Object;
   function To_Object (Value : in Long_Integer) return Object;
   function To_Object (Value : in Long_Long_Integer) return Object;
   function To_Object (Value : in Float) return Object;
   function To_Object (Value : in Long_Float) return Object;
   function To_Object (Value : in Long_Long_Float) return Object;
   function To_Object (Value : in String) return Object;
   function To_Object (Value : in Wide_Wide_String) return Object;
   function To_Object (Value : in Unbounded_String) return Object;
   function To_Object (Value : in Unbounded_Wide_Wide_String) return Object;
   function To_Object (Value : in Boolean) return Object;
   function To_Object (Value : in Ada.Calendar.Time) return Object;
   function To_Object (Value : access EL.Beans.Readonly_Bean'Class) return Object;

   function To_Object (Type_Def : in Object_Type_Access;
                       Value    : in System.Address) return Object;

   --  Comparison of objects
   function "<" (Left, Right : Object) return Boolean;
   function "<=" (Left, Right : Object) return Boolean;
   function ">" (Left, Right : Object) return Boolean;
   function ">=" (Left, Right : Object) return Boolean;
   function "=" (Left, Right : Object) return Boolean;

   --  Arithmetic operations on objects
   function "+" (Left, Right : Object) return Object;
   function "-" (Left, Right : Object) return Object;
   function "*" (Left, Right : Object) return Object;
   function "/" (Left, Right : Object) return Object;
   function "&" (Left, Right : Object) return Object;
   function "mod" (Left, Right : Object) return Object;
   function "-" (Left : Object) return Object;

private

   type Object (Of_Type : Data_Type := TYPE_NULL) is record
      Type_Def : Object_Type_Access := null;
      case Of_Type is
         when TYPE_NULL =>
            null;

         when TYPE_INTEGER =>
            Int_Value : Long_Long_Integer;

         when TYPE_BOOLEAN =>
            Bool_Value : Boolean;

         when TYPE_FLOAT =>
            Float_Value : Long_Long_Float;

         when TYPE_STRING =>
            String_Value : Unbounded_String;

         when TYPE_WIDE_STRING =>
            Wide_String_Value : Unbounded_Wide_Wide_String;

         when TYPE_TIME =>
            Time_Value : Ada.Calendar.Time;

         when TYPE_EXTERNAL =>
            Addr : System.Address;

         when TYPE_BEAN =>
            Bean : access EL.Beans.Readonly_Bean'Class;

      end case;
   end record;

   Null_Object : constant Object := Object '(Of_Type => TYPE_NULL, Type_Def => null);

end EL.Objects;

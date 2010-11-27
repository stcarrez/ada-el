-----------------------------------------------------------------------
--  EL.Objects.Enums -- Helper conversion for discrete types
--  Copyright (C) 2010 Stephane Carrez
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
with Ada.Characters.Conversions;
package body EL.Objects.Enums is

   use Ada.Characters.Conversions;

   Value_Range : constant Long_Long_Integer := T'Pos (T'Last) - T'Pos (T'First) + 1;

   --  ------------------------------
   --  Integer Type
   --  ------------------------------
   type Enum_Type is new Int_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Enum_Type) return String;

   overriding
   function To_String (Type_Def : in Enum_Type;
                       Value    : in Object_Value) return String;


   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : Enum_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Enum";
   end Get_Name;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Enum_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      return T'Image (T'Val (Value.Int_Value));
   end To_String;

   Value_Type  : aliased constant Enum_Type := Enum_Type '(others => <>);

   --  ------------------------------
   --  Create an object from the given value.
   --  ------------------------------
   function To_Object (Value : in T) return Object is
   begin
      return Object '(Controlled with
        V => Object_Value '(Of_Type   => TYPE_INTEGER,
                            Int_Value => Long_Long_Integer (T'Pos (Value))),
        Type_Def  => Value_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert the object into a value.
   --  Raises Constraint_Error if the object cannot be converter to the target type.
   --  ------------------------------
   function To_Value (Value : in EL.Objects.Object) return T is
   begin
      case Value.V.Of_Type is
         when TYPE_INTEGER =>
            if ROUND_VALUE then
               return T'Val (Value.V.Int_Value mod Value_Range);
            else
               return T'Val (Value.V.Int_Value);
            end if;

         when TYPE_BOOLEAN =>
            return T'Val (Boolean'Pos (Value.V.Bool_Value));

         when TYPE_FLOAT =>
            if ROUND_VALUE then
               return T'Val (To_Long_Long_Integer (Value) mod Value_Range);
            else
               return T'Val (To_Long_Long_Integer (Value));
            end if;

         when TYPE_STRING =>
            if Value.V.Proxy = null then
               raise Constraint_Error with "The object value is null";
            end if;
            return T'Value (Value.V.Proxy.String_Value.all);

         when TYPE_WIDE_STRING =>
            if Value.V.Proxy = null then
               raise Constraint_Error with "The object value is null";
            end if;
            return T'Value (To_String (Value.V.Proxy.Wide_String_Value.all));

         when TYPE_NULL =>
            raise Constraint_Error with "The object value is null";

         when TYPE_TIME =>
            raise Constraint_Error with "Cannot convert a date into a discrete type";

         when TYPE_BEAN =>
            raise Constraint_Error with "Cannot convert a bean into a discrete type";

      end case;
   end To_Value;

end EL.Objects.Enums;

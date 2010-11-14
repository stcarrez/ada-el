-----------------------------------------------------------------------
--  EL.Objects.Hash -- Hash on an object
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

with Ada.Strings.Hash;
with Ada.Characters.Conversions;
with Ada.Unchecked_Conversion;
with Interfaces;
with EL.Beans;
function EL.Objects.Hash (Key : in Object) return Ada.Containers.Hash_Type is
   use Ada.Containers;
   use Ada.Strings;
   use Ada.Characters.Conversions;
   use Interfaces;
   use EL.Beans;

   type Unsigned_32_Array is array (Natural range <>) of Unsigned_32;

   subtype U32_For_Float is Unsigned_32_Array (1 .. Long_Long_Float'Size / 32);

   subtype U32_For_Long is Unsigned_32_Array (1 .. Long_Long_Integer'Size / 32);

   subtype U32_For_Access is Unsigned_32_Array (1 .. Readonly_Bean_Access'Size / 32);

   --  Hash the integer and floats using 32-bit values.
   function To_U32_For_Long is new Ada.Unchecked_Conversion (Source => Long_Long_Integer,
                                                             Target => U32_For_Long);

   --  Likewise for floats.
   function To_U32_For_Float is new Ada.Unchecked_Conversion (Source => Long_Long_Float,
                                                              Target => U32_For_Float);

   --  Likewise for the bean pointer
   function To_U32_For_Access is new Ada.Unchecked_Conversion (Source => Readonly_Bean_Access,
                                                               Target => U32_For_Access);

   Value : Unsigned_64 := 0;
begin
   case key.V.Of_Type is
      when TYPE_NULL =>
         return 0;

      when TYPE_BOOLEAN =>
         if Key.V.Bool_Value then
            return 1;
         else
            return 2;
         end if;

      when TYPE_INTEGER =>
         declare
            U32 : constant U32_For_Long :=  To_U32_For_Long (Key.V.Int_Value);
            Val : Unsigned_32 := U32 (U32'First);
         begin
            for I in U32'First + 1 .. U32'Last loop
               Val := Val xor U32 (I);
            end loop;
            return Hash_Type (Val);
         end;

      when TYPE_FLOAT =>
         declare
            U32 : constant U32_For_Float :=  To_U32_For_Float (Key.V.Float_Value);
            Val : Unsigned_32 := U32 (U32'First);
         begin
            for I in U32'First + 1 .. U32'Last loop
               Val := Val xor U32 (I);
            end loop;
            return Hash_Type (Val);
         end;

      when TYPE_STRING =>
         if Key.V.Proxy = null then
            return 0;
         else
            return Hash (Key.V.Proxy.String_Value.all);
         end if;

      when TYPE_TIME =>
         declare
            U32 : constant U32_For_Long :=  To_U32_For_Long (Key.V.Time_Value);
            Val : Unsigned_32 := U32 (U32'First);
         begin
            for I in U32'First + 1 .. U32'Last loop
               Val := Val xor U32 (I);
            end loop;
            return Hash_Type (Val);
         end;

      when TYPE_WIDE_STRING =>
         if Key.V.Proxy = null then
            return 0;
         else
            return Hash (To_String (Key.V.Proxy.Wide_String_Value.all));
         end if;

      when TYPE_BEAN =>
         if Key.V.Proxy.Bean = null then
            return 0;
         end if;
         declare
            U32 : constant U32_For_Access :=  To_U32_For_Access (Key.V.Proxy.Bean.all'Access);
            Val : Unsigned_32 := U32 (U32'First);

            --  The loop is not executed if pointers are 32-bit wide.
            pragma Warnings (Off);
         begin
            for I in U32'First + 1 .. U32'Last loop
               Val := Val xor U32 (I);
            end loop;
            return Hash_Type (Val);
         end;

   end case;

   --  Hash the 64-bit value into a 32-bit result that fits in Hash_Type.
   Value := (Value and 16#0ffffffff#) xor (Shift_Right (Value, 32));
   return Hash_Type (Value);
end EL.Objects.Hash;

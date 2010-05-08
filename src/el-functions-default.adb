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

package body EL.Functions.Default is

   --  ------------------------------
   --  Default Function mapper
   --  ------------------------------
   --

   use Function_Maps;

   --  ------------------------------
   --  Find the function knowing its name.
   --  ------------------------------
   function Get_Function (Mapper    : Default_Function_Mapper;
                          Namespace : String;
                          Name      : String) return Function_Access is
      Full_Name : constant String := Namespace & ":" & Name;
      C : constant Cursor := Mapper.Map.Find (Key => To_Unbounded_String (Full_Name));
   begin
      if Has_Element (C) then
         return Element (C);
      end if;
      raise No_Function;
   end Get_Function;

   --  ------------------------------
   --  Bind a name to a function.
   --  ------------------------------
   procedure Set_Function (Mapper    : in out Default_Function_Mapper;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_Access) is
      Full_Name : constant String := Namespace & ":" & Name;
   begin
      Mapper.Map.Include (Key => To_Unbounded_String (Full_Name), New_Item => Func);
   end Set_Function;

   --  ------------------------------
   --  Truncate the string representation represented by <b>Value</b> to
   --  the length specified by <b>Size</b>.
   --  ------------------------------
   function Truncate (Value : EL.Objects.Object;
                      Size  : EL.Objects.Object) return EL.Objects.Object is
      Cnt : constant Integer := To_Integer (Size);
   begin
      if Cnt <= 0 then
         return To_Object (String '(""));
      end if;
      if Get_Type (Value) = TYPE_WIDE_STRING then
         declare
            S : constant Wide_Wide_String := To_Wide_Wide_String (Value);
         begin
            return To_Object (S (1 .. Cnt));
         end;
      else
         --  Optimized case: use a String if we can.
         declare
            S : constant String := To_String (Value);
         begin
            return To_Object (S (1 .. Cnt));
         end;
      end if;
   end Truncate;

end EL.Functions.Default;

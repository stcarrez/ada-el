-----------------------------------------------------------------------
--  el-functions-default -- Default function mapper
--  Copyright (C) 2009, 2010, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
   overriding
   function Get_Function (Mapper    : Default_Function_Mapper;
                          Namespace : String;
                          Name      : String) return Function_Access is
      Full_Name : constant String := Namespace & ":" & Name;
      C : constant Cursor := Mapper.Map.Find (Key => Full_Name);
   begin
      if Has_Element (C) then
         return Element (C);
      end if;
      raise No_Function with "Function '" & Full_Name & "' not found";
   end Get_Function;

   --  ------------------------------
   --  Bind a name to a function.
   --  ------------------------------
   overriding
   procedure Set_Function (Mapper    : in out Default_Function_Mapper;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_Access) is
      Full_Name : constant String := Namespace & ":" & Name;
   begin
      Mapper.Map.Include (Key => Full_Name, New_Item => Func);
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

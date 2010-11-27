-----------------------------------------------------------------------
--  EL.Functions -- Functions to be plugged in expressions
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

package body EL.Functions is

   --  ------------------------------
   --  Bind a name to a function.
   --  ------------------------------
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_1_Access) is
   begin
      Mapper.Set_Function (Namespace, Name,
                           Function_Access '(Of_Type => F_1_ARG, Func1 => Func));
   end Set_Function;

   --  ------------------------------
   --  Bind a name to a function.
   --  ------------------------------
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_2_Access) is
   begin
      Mapper.Set_Function (Namespace, Name,
                           Function_Access '(Of_Type => F_2_ARG, Func2 => Func));
   end Set_Function;

   --  ------------------------------
   --  Bind a name to a function.
   --  ------------------------------
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_3_Access) is
   begin
      Mapper.Set_Function (Namespace, Name,
                           Function_Access '(Of_Type => F_3_ARG, Func3 => Func));
   end Set_Function;

   --  ------------------------------
   --  Bind a name to a function.
   --  ------------------------------
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_4_Access) is
   begin
      Mapper.Set_Function (Namespace, Name,
                           Function_Access '(Of_Type => F_4_ARG, Func4 => Func));
   end Set_Function;

   --  ------------------------------
   --  Register some pre-defined functions in the function mapper.
   --  ------------------------------
   procedure Register_Predefined (Mapper : in out Function_Mapper'Class) is
   begin
      Mapper.Set_Function ("", "int", EL.Objects.Cast_Integer'Access);
      Mapper.Set_Function ("", "float", EL.Objects.Cast_Float'Access);
--        Mapper.Set_Function ("", "time", EL.Objects.Time.Cast_Time'Access);
--        Mapper.Set_Function ("", "date", EL.Objects.Time.Cast_Time'Access);
      Mapper.Set_Function ("", "string", EL.Objects.Cast_String'Access);
   end Register_Predefined;

end EL.Functions;

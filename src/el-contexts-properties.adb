-----------------------------------------------------------------------
--  el-contexts-properties -- EL Resolver using util properties
--  Copyright (C) 2011, 2021 Stephane Carrez
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

with Util.Beans.Objects;
package body EL.Contexts.Properties is

   --  ------------------------------
   --  Set the properties used for resolving values.
   --  ------------------------------
   procedure Set_Properties (Resolver   : in out Property_Resolver;
                             Properties : in Util.Properties.Manager'Class) is
   begin
      Resolver.Props := Util.Properties.Manager (Properties);
   end Set_Properties;

   --  ------------------------------
   --  Get the value associated with a base object and a given property.
   --  ------------------------------
   overriding
   function Get_Value (Resolver : in Property_Resolver;
                       Context  : in ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : in Unbounded_String) return EL.Objects.Object is
      pragma Unreferenced (Context);
   begin
      if Base /= null then
         return Base.Get_Value (To_String (Name));

      elsif Resolver.Props.Exists (Name) then
         return Util.Beans.Objects.To_Object (String '(Resolver.Props.Get (Name)));

      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value associated with a base object and a given property.
   --  ------------------------------
   overriding
   procedure Set_Value (Resolver : in out Property_Resolver;
                        Context  : in ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in EL.Objects.Object) is
      pragma Unreferenced (Context);
   begin
      if Base /= null then
         Base.Set_Value (To_String (Name), Value);
      else
         Resolver.Props.Set (Name, Util.Beans.Objects.To_Unbounded_String (Value));
      end if;
   end Set_Value;

end EL.Contexts.Properties;

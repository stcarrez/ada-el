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

with EL.Objects;
with Util.Beans.Basic;
with Util.Properties;
package EL.Contexts.Properties is

   --  ------------------------------
   --  Property Resolver
   --  ------------------------------
   --  The <b>Property_Resolver</b> uses a property manager to resolve names.
   type Property_Resolver is new ELResolver with private;
   type Property_Resolver_Access is access all Property_Resolver'Class;

   --  Set the properties used for resolving values.
   procedure Set_Properties (Resolver   : in out Property_Resolver;
                             Properties : in Util.Properties.Manager'Class);

   --  Get the value associated with a base object and a given property.
   overriding
   function Get_Value (Resolver : in Property_Resolver;
                       Context  : in ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : in Unbounded_String) return EL.Objects.Object;

   --  Set the value associated with a base object and a given property.
   overriding
   procedure Set_Value (Resolver : in out Property_Resolver;
                        Context  : in ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in EL.Objects.Object);

private

   type Property_Resolver is new ELResolver with record
      Props : Util.Properties.Manager;
   end record;

end EL.Contexts.Properties;

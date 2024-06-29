-----------------------------------------------------------------------
--  el-contexts-properties -- EL Resolver using util properties
--  Copyright (C) 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

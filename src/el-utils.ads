-----------------------------------------------------------------------
--  el-utils -- Utilities around EL
--  Copyright (C) 2011, 2012, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Properties;
with Util.Beans.Objects;

with EL.Contexts;
package EL.Utils is

   --  Expand the properties stored in <b>Source</b> by evaluating the EL expressions
   --  used in the property values.  The EL context passed in <b>Context</b> can be used
   --  to specify the EL functions or some pre-defined beans that could be used.
   --  The EL context will integrate the source properties as well as properties stored
   --  in <b>Into</b> (only the <b>Source</b> properties will be evaluated).
   procedure Expand (Source  : in Util.Properties.Manager'Class;
                     Into    : in out Util.Properties.Manager'Class;
                     Context : in EL.Contexts.ELContext'Class);
   procedure Expand (Config  : in out Util.Properties.Manager'Class;
                     Context : in EL.Contexts.ELContext'Class);

   --  Evaluate the possible EL expressions used in <b>Value</b> and return the
   --  string that correspond to that evaluation.
   function Eval (Value   : in String;
                  Context : in EL.Contexts.ELContext'Class) return String;

   --  Evaluate the possible EL expressions used in <b>Value</b> and return an
   --  object that correspond to that evaluation.
   function Eval (Value   : in String;
                  Context : in EL.Contexts.ELContext'Class) return Util.Beans.Objects.Object;

   --  Evaluate the possible EL expressions used in <b>Value</b> and return an
   --  object that correspond to that evaluation.
   function Eval (Value   : in Util.Beans.Objects.Object;
                  Context : in EL.Contexts.ELContext'Class) return Util.Beans.Objects.Object;

end EL.Utils;

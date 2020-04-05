-----------------------------------------------------------------------
--  el-utils -- Utilities around EL
--  Copyright (C) 2011, 2012, 2020 Stephane Carrez
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

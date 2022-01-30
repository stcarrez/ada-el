-----------------------------------------------------------------------
--  el-contexts-tls -- EL context and Thread Local Support
--  Copyright (C) 2015, 2021 Stephane Carrez
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
with EL.Contexts.Default;

--  == EL TLS Context ==
--  The <tt>TLS_Context</tt> type defines an expression context that associates itself to
--  a per-thread context information.  By declaring a variable with this type, the expression
--  context is associated with the current thread and it can be retrieved at any time by using the
--  <tt>Current</tt> function.  As soon as the variable is finalized, the current thread context
--  is updated.
--
--  The expression context may be declared as follows:
--
--     Context : EL.Contexts.TLS.TLS_Context;
--
--  And at any time, a function or procedure that needs to evaluate an expression can use it:
--
--    Expr : EL.Expressions.Expression;
--    ...
--    Value : Util.Beans.Objects.Object := Expr.Get_Value (EL.Contexts.TLS.Current.all);
--
package EL.Contexts.TLS is

   --  ------------------------------
   --  TLS Context
   --  ------------------------------
   --  Context information for expression evaluation.
   type TLS_Context is new EL.Contexts.Default.Default_Context with private;

   --  Get the current EL context associated with the current thread.
   function Current return EL.Contexts.ELContext_Access;

private

   type TLS_Context is new EL.Contexts.Default.Default_Context with record
      Previous    : EL.Contexts.ELContext_Access;
   end record;

   --  Initialize and setup a new per-thread EL context.
   overriding
   procedure Initialize (Obj : in out TLS_Context);

   --  Restore the previous per-thread EL context.
   overriding
   procedure Finalize (Obj : in out TLS_Context);

end EL.Contexts.TLS;

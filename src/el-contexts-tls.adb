-----------------------------------------------------------------------
--  el-contexts-tls -- EL context and Thread Local Support
--  Copyright (C) 2015, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body EL.Contexts.TLS is

   Context : EL.Contexts.ELContext_Access := null with Thread_Local_Storage;
--   pragma Thread_Local_Storage (Context);

   --  ------------------------------
   --  Get the current EL context associated with the current thread.
   --  ------------------------------
   function Current return EL.Contexts.ELContext_Access is
   begin
      return Context;
   end Current;

   --  ------------------------------
   --  Initialize and setup a new per-thread EL context.
   --  ------------------------------
   overriding
   procedure Initialize (Obj : in out TLS_Context) is
   begin
      Obj.Previous := Context;
      Context := Obj'Unchecked_Access;
      EL.Contexts.Default.Default_Context (Obj).Initialize;
   end Initialize;

   --  ------------------------------
   --  Restore the previous per-thread EL context.
   --  ------------------------------
   overriding
   procedure Finalize (Obj : in out TLS_Context) is
   begin
      Context := Obj.Previous;
      EL.Contexts.Default.Default_Context (Obj).Finalize;
   end Finalize;

end EL.Contexts.TLS;

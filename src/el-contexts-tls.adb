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

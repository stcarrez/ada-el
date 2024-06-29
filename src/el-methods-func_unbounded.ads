-----------------------------------------------------------------------
--  el-methods-func_unbounded -- Pre-defined binding
--  Copyright (C) 2010, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with EL.Methods.Func_1;

--  This package provides the method bindings to invoke
--  methods with the following signature:
--
--    function F (Object : <Bean>;
--                Param  : Unbounded_String)
--             return Unbounded_String;
--
--  Example of call:
--
--    Ctx : ELContext := ...;
--    M   : Method_Expression := ...;
--    A   : Unbounded_String  := ...;
--
--    R : Unbounded_String := Func_Unbounded.Execute (M, A, Ctx);
--
package EL.Methods.Func_Unbounded is
  new EL.Methods.Func_1 (Param1_Type => Ada.Strings.Unbounded.Unbounded_String,
                         Return_Type => Ada.Strings.Unbounded.Unbounded_String);

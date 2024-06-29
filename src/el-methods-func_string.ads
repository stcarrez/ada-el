-----------------------------------------------------------------------
--  el-methods-func_string -- Pre-defined binding
--  Copyright (C) 2010, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with EL.Methods.Func_1;

--  This package provides the method bindings to invoke
--  methods with the following signature:
--
--    function F (Object : <Bean>;
--                Param  : String)
--             return String;
--
--  Example of call:
--
--    Ctx : ELContext := ...;
--    M   : Method_Expression := ...;
--    A   : String  := ...;
--
--    R : String := Func_Unbounded.Execute (M, A, Ctx);
--
package EL.Methods.Func_String is
  new EL.Methods.Func_1 (Param1_Type => String,
                         Return_Type => String);

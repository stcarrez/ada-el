-----------------------------------------------------------------------
--  el-methods-func_string -- Pre-defined binding
--  Copyright (C) 2010, 2021 Stephane Carrez
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

-----------------------------------------------------------------------
--  el-functions -- Functions to be plugged in expressions
--  Copyright (C) 2009, 2010, 2012, 2021, 2022 Stephane Carrez
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

--
--  The expression context provides information to resolve runtime
--  information when evaluating an expression.  The context provides
--  a resolver whose role is to find variables given their name.

with EL.Objects;

package EL.Functions is

   pragma Preelaborate;

   use EL.Objects;

   No_Function : exception;

   type Function_Type is (F_1_ARG, F_2_ARG, F_3_ARG, F_4_ARG);

   --  Functions that can be plugged in a context and which can later
   --  be used in an expression.
   type Function_1_Access is access function (P : Object) return Object;
   type Function_2_Access is access function (P1, P2 : Object) return Object;
   type Function_3_Access is access function (P1, P2, P3 : Object) return Object;
   type Function_4_Access is access function (P1, P2, P3, P4 : Object) return Object;

   type Function_Access (Of_Type : Function_Type := F_1_ARG) is record
      Optimize : Boolean := True;
      case Of_Type is
         when F_1_ARG =>
            Func1 : Function_1_Access;

         when F_2_ARG =>
            Func2 : Function_2_Access;

         when F_3_ARG =>
            Func3 : Function_3_Access;

         when F_4_ARG =>
            Func4 : Function_4_Access;

      end case;
   end record;

   --  ------------------------------
   --  Function mapper
   --  ------------------------------
   --
   type Function_Mapper is interface;
   type Function_Mapper_Access is access all Function_Mapper'Class;

   --  Find the function knowing its name.
   function Get_Function (Mapper    : Function_Mapper;
                          Namespace : String;
                          Name      : String) return Function_Access is abstract;

   --  Bind a name to a function in the given namespace.
   procedure Set_Function (Mapper    : in out Function_Mapper;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_Access) is abstract;

   --  Bind a name to a function.
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_1_Access;
                           Optimize  : in Boolean := True);

   --  Bind a name to a function.
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_2_Access;
                           Optimize  : in Boolean := True);

   --  Bind a name to a function.
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_3_Access;
                           Optimize  : in Boolean := True);

   --  Bind a name to a function.
   procedure Set_Function (Mapper    : in out Function_Mapper'Class;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_4_Access;
                           Optimize  : in Boolean := True);

end EL.Functions;

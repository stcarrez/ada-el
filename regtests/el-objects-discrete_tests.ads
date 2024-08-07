-----------------------------------------------------------------------
--  el-objects-tests - Generic simple test for discrete object types
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with EL.Objects;

generic
   type Test_Type is private;
   with function To_Type (Obj : EL.Objects.Object) return Test_Type is <>;
   with function To_Object_Test (V : Test_Type) return EL.Objects.Object is <>;
   with function "-" (Left, Right : Test_Type) return Test_Type is <>;
   with function "+" (Left, Right : Test_Type) return Test_Type is <>;
   with function "=" (Left, Right : Test_Type) return Boolean is <>;
   with function Value (S : String) return Test_Type;
   Test_Name   : String;
   Test_Values : String;
package EL.Objects.Discrete_Tests is

   type Test is new Util.Tests.Test with record
      I1 : Integer;
      I2 : Integer;
   end record;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

end EL.Objects.Discrete_Tests;

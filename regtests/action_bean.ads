-----------------------------------------------------------------------
--  Action_Bean - Simple bean with methods that can be called from EL
--  Copyright (C) 2010, 2022 Stephane Carrez
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
with EL.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;
with EL.Methods.Proc_1;
with EL.Methods.Proc_2;
with Test_Bean;
with Ada.Unchecked_Deallocation;
package Action_Bean is

   type Action is new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with record
      Person     : Test_Bean.Person;
      Count      : Natural;
   end record;
   type Action_Access is access all Action'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : Action; Name : String) return EL.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Action;
                        Name  : in String;
                        Value : in EL.Objects.Object);

   --  Get the EL method bindings exposed by the Action type.
   overriding
   function Get_Method_Bindings (From : in Action)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Action with one parameter.
   --  Sets the person.
   procedure Notify (Target : in out Action;
                     Param  : in out Test_Bean.Person);

   --  Action with two parameters.
   --  Sets the person object and the counter.
   procedure Notify (Target : in out Action;
                     Param  : in Test_Bean.Person;
                     Count  : in Natural);

   --  Action with one parameter
   procedure Print (Target : in out Action;
                    Param  : in out Test_Bean.Person);

   --  Package to invoke an EL method with one <b>Person</b> as parameter.
   package Proc_Action is new EL.Methods.Proc_1 (Param1_Type => Test_Bean.Person);

   --  Package to invoke an EL method with <b>Person</b> as parameter and a <b>Natural</b>.
   package Proc2_Action is
     new EL.Methods.Proc_2 (Param1_Type => Test_Bean.Person,
                            Param2_Type => Natural);

   procedure Free is new Ada.Unchecked_Deallocation (Object => Action'Class,
                                                     Name   => Action_Access);

end Action_Bean;

-----------------------------------------------------------------------
--  Action_Bean - Simple bean with methods that can be called from EL
--  Copyright (C) 2010, 2011, 2022 Stephane Carrez
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

package body Action_Bean is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : Action; Name : String) return EL.Objects.Object is
   begin
      if Name = "count" then
         return EL.Objects.To_Object (From.Count);
      end if;
      return From.Person.Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Action;
                        Name  : in String;
                        Value : in EL.Objects.Object) is
   begin
      From.Person.Set_Value (Name, Value);
   end Set_Value;

   --  ------------------------------
   --  Action with one parameter.
   --  Sets the person.
   --  ------------------------------
   procedure Notify (Target : in out Action;
                     Param  : in out Test_Bean.Person) is
   begin
      Target.Person := Param;
   end Notify;

   --  ------------------------------
   --  Action with two parameters.
   --  Sets the person object and the counter.
   --  ------------------------------
   procedure Notify (Target : in out Action;
                     Param  : in Test_Bean.Person;
                     Count  : in Natural) is
   begin
      Target.Person := Param;
      Target.Count  := Count;
   end Notify;

   --  ------------------------------
   --  Action with one parameter
   --  ------------------------------
   procedure Print (Target : in out Action;
                    Param  : in out Test_Bean.Person) is
   begin
      Target.Person := Param;
   end Print;

   package Notify_Binding is
     new Proc_Action.Bind (Bean => Action, Method => Notify, Name => "notify");

   package Notify_Count_Binding is
     new Proc2_Action.Bind (Bean => Action, Method => Notify, Name => "notify2");

   package Print_Binding is
     new Proc_Action.Bind (Bean => Action, Method => Print, Name => "print");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Notify_Binding.Proxy'Access,
         Print_Binding.Proxy'Access,
         Notify_Count_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the EL method bindings exposed by the Action type.
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Action)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

end Action_Bean;

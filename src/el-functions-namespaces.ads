-----------------------------------------------------------------------
--  el-functions-namespaces -- Namespace function mapper
--  Copyright (C) 2011, 2021 Stephane Carrez
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

private with Util.Strings.Maps;
package EL.Functions.Namespaces is

   --  ------------------------------
   --  Namespace Function mapper
   --  ------------------------------
   --  The <b>NS_Function_Mapper</b> is a delegate function mapper which provides XML
   --  namespace support.  The XML namespaces are registered with the <b>Set_Namespace</b>
   --  procedure which records a prefix and the associated URI namespace.  When a function is
   --  searched, the <b>Namespace</b> is searched in the prefix map to find the corresponding
   --  URI namespace.  Then, the delegated function mapper is invoked using that URI.
   type NS_Function_Mapper is new Function_Mapper with private;
   type Function_Mapper_Access is access all Function_Mapper'Class;

   --  Find the function knowing its name.
   overriding
   function Get_Function (Mapper    : in NS_Function_Mapper;
                          Namespace : in String;
                          Name      : in String) return Function_Access;

   --  Bind a name to a function in the given namespace.
   overriding
   procedure Set_Function (Mapper    : in out NS_Function_Mapper;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_Access);

   --  Associate the <b>Prefix</b> with the givien <b>URI</b> building a new namespace.
   procedure Set_Namespace (Mapper : in out NS_Function_Mapper;
                            Prefix : in String;
                            URI    : in String);

   --  Remove the namespace prefix binding.
   procedure Remove_Namespace (Mapper : in out NS_Function_Mapper;
                               Prefix : in String);

   --  Set the delegate function mapper.
   procedure Set_Function_Mapper (Mapper   : in out NS_Function_Mapper;
                                  Delegate : in Function_Mapper_Access);

private

   type NS_Function_Mapper is new Function_Mapper with record
      Mapping : Util.Strings.Maps.Map;
      Mapper  : Function_Mapper_Access;
   end record;

end EL.Functions.Namespaces;

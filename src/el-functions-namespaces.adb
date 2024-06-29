-----------------------------------------------------------------------
--  el-functions-namespaces -- Namespace function mapper
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Log.Loggers;
package body EL.Functions.Namespaces is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("EL.Functions.Namespaces");

   --  ------------------------------
   --  Find the function knowing its name.
   --  ------------------------------
   overriding
   function Get_Function (Mapper    : in NS_Function_Mapper;
                          Namespace : in String;
                          Name      : in String) return Function_Access is
      use Util.Strings.Maps;

      Pos : constant Cursor := Mapper.Mapping.Find (Namespace);
   begin
      if Has_Element (Pos) then
         return Mapper.Mapper.Get_Function (Element (Pos), Name);
      end if;
      raise No_Function with "Function '" & Namespace & ':' & Name & "' not found";
   end Get_Function;

   --  ------------------------------
   --  Bind a name to a function in the given namespace.
   --  ------------------------------
   overriding
   procedure Set_Function (Mapper    : in out NS_Function_Mapper;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_Access) is
   begin
      null;
   end Set_Function;

   --  ------------------------------
   --  Associate the <b>Prefix</b> with the givien <b>URI</b> building a new namespace.
   --  ------------------------------
   procedure Set_Namespace (Mapper : in out NS_Function_Mapper;
                            Prefix : in String;
                            URI    : in String) is
   begin
      Log.Debug ("Add namespace {0}:{1}", Prefix, URI);

      Mapper.Mapping.Include (Prefix, URI);
   end Set_Namespace;

   --  ------------------------------
   --  Remove the namespace prefix binding.
   --  ------------------------------
   procedure Remove_Namespace (Mapper : in out NS_Function_Mapper;
                               Prefix : in String) is
      use Util.Strings.Maps;
      Pos : Cursor := Mapper.Mapping.Find (Prefix);
   begin
      Log.Debug ("Remove namespace {0}", Prefix);

      if Has_Element (Pos) then
         Mapper.Mapping.Delete (Pos);
      end if;
   end Remove_Namespace;

   --  ------------------------------
   --  Set the delegate function mapper.
   --  ------------------------------
   procedure Set_Function_Mapper (Mapper   : in out NS_Function_Mapper;
                                  Delegate : in Function_Mapper_Access) is
   begin
      Mapper.Mapper := Delegate;
   end Set_Function_Mapper;

end EL.Functions.Namespaces;

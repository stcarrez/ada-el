-----------------------------------------------------------------------
--  methods -- Example of Method_Expression
--  Copyright (C) 2010 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  This file is part of ASF.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2,
--  or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330,
--  Boston, MA 02111-1307, USA.
-----------------------------------------------------------------------
with EL.Expressions;
with EL.Objects;
with EL.Contexts.Default;
with EL.Beans;
with EL.Beans.Methods;
with EL.Beans.Methods.Func_String;
with Ada.Text_IO;
with Bean;
procedure Methods is

   use Bean;
   use EL.Beans.Methods;

   Joe    : Person_Access := Create_Person ("Joe", "Smith", 12);
   Bill   : Person_Access := Create_Person ("Bill", "Johnson", 42);

   Ctx    : EL.Contexts.Default.Default_Context;
   Method : EL.Expressions.Method_Expression;

   procedure Evaluate (Ctx : EL.Contexts.ELContext'Class;
                       Msg : in String) is
      Result : constant String := Func_String.Execute (Method, Msg, Ctx);
   begin
      Ada.Text_IO.Put_Line ("#{user.print} returned: " & Result);
   end Evaluate;

begin
   --  Get a method expression to invoke 'print' on the 'user' bean.
   --  The method expression does not know anything about 'user' (Joe or Bill).
   Method := EL.Expressions.Create_Expression ("#{user.print}", Ctx);

   --  Bind the context to 'Joe' and evaluate.
   Ctx.Set_Variable ("user", Joe);
   Evaluate (Ctx, "This is Joe");

   --  Bind the context to 'Bill' and evaluate.
   Ctx.Set_Variable ("user", Bill);
   Evaluate (Ctx, "This is Bill");

   Free (Joe);
   Free (Bill);
end Methods;

-----------------------------------------------------------------------
--  methods -- Example of Method_Expression
--  Copyright (C) 2010, 2022 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Expressions;
with EL.Contexts.Default;
with EL.Methods;
with EL.Methods.Func_String;
with Ada.Text_IO;
with Bean;
procedure Methods is

   procedure Evaluate (Ctx : EL.Contexts.ELContext'Class;
                       Msg : in String);

   use Bean;
   use EL.Methods;

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

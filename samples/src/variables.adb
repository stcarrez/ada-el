-----------------------------------------------------------------------
--  el -- Evaluate an EL expression
--  Copyright (C) 2009, 2010 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Expressions;
with EL.Objects;
with EL.Contexts.Default;
with Ada.Text_IO;
with Bean;
procedure Variables is

   use Bean;

   Joe    : Person_Access := Create_Person ("Joe", "Smith", 12);
   Bill   : Person_Access := Create_Person ("Bill", "Johnson", 42);
   Ctx    : EL.Contexts.Default.Default_Context;
   E      : EL.Expressions.Expression;
   VE     : EL.Expressions.Value_Expression;
   Result : EL.Objects.Object;

begin
   E := EL.Expressions.Create_Expression ("#{user.firstName} #{user.lastName}", Ctx);

   --  Bind the context to 'Joe' and evaluate
   Ctx.Set_Variable ("user", Joe);
   Result := E.Get_Value (Ctx);
   Ada.Text_IO.Put_Line ("User name is " & EL.Objects.To_String (Result));

   --  Bind the context to 'Bill' and evaluate
   Ctx.Set_Variable ("user", Bill);
   Result := E.Get_Value (Ctx);
   Ada.Text_IO.Put_Line ("User name is " & EL.Objects.To_String (Result));

   --  Create a value expression to change the user firstName property.
   VE := EL.Expressions.Create_Expression ("#{user.firstName}", Ctx);

   --  Change the user first name by setting the value expression.
   VE.Set_Value (Context => Ctx, Value => EL.Objects.To_Object (String '("Harold")));

   Result := E.Get_Value (Ctx);
   Ada.Text_IO.Put_Line ("User name is " & EL.Objects.To_String (Result));

   Free (Joe);
   Free (Bill);
end Variables;

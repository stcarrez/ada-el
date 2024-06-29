-----------------------------------------------------------------------
--  el -- Evaluate an EL expression
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Expressions;
with EL.Objects;
with EL.Contexts.Default;
with Ada.Text_IO;

procedure Evaluate is
   Expr   : constant String := "#{1 + (2 - 3) * 4}";
   Ctx    : EL.Contexts.Default.Default_Context;
   E      : EL.Expressions.Expression;
   Result : EL.Objects.Object;
begin
   Ada.Text_IO.Put_Line ("Evaluate: " & Expr);
   E := EL.Expressions.Create_Expression (Expr, Ctx);
   Result := E.Get_Value (Ctx);
   Ada.Text_IO.Put_Line ("Result: " & EL.Objects.To_String (Result));
end Evaluate;

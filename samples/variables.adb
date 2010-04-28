-----------------------------------------------------------------------
--  el -- Evaluate an EL expression
--  Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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
with Ada.Text_IO;
with Bean;
with Ada.Calendar;
procedure Variable is
   
   use Bean;
   use Ada.Text_IO;
   use EL.Expressions;
   use EL.Objects;
      use Ada.Calendar;

   E : Expression;

   Ctx : EL.Contexts.Default.Default_Context;
   
   Joe  : Person_Access := Create_Person ("Joe", "Smith", 12);
   Bill : Person_Access := Create_Person ("Bill", "Johnson", 42);
      Start : Time;
      
   Result : Object;
begin
   E := Create_Expression ("user.firstName & ' ' & user.lastName");

   --  Bind the context to 'Joe' and evaluate
   Ctx.Set_Variable ("user", Joe);
   Start := Ada.Calendar.Clock;
   for I in 1 .. 1_000_000 loop
      Result := E.Get_Value (Ctx);
   end loop;
   Put_Line ("Time to eval: " & Duration'Image (Clock - Start));
   Put_Line ("Joe's name is " & To_String (Result));
   
   --  Bind the context to 'Bill' and evaluate
   Ctx.Set_Variable ("user", Bill);
   Result := E.Get_Value (Ctx);
   Put_Line ("Bill's name is " & To_String (Result));
end Variable;

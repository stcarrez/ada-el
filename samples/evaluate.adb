-----------------------------------------------------------------------
--  el -- Evaluate an EL expression
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with EL.Expressions;
with EL.Objects;
with EL.Contexts.Default;
with Ada.Text_IO;
with Ada.Calendar;

procedure Evaluate is

   use Ada.Calendar;
   use Ada.Text_IO;
   use EL.Expressions;
   use EL.Objects;

   E : Expression;

   Ctx : EL.Contexts.Default.Default_Context;
   Start : Time;
   Result : Object;
   D : Duration;
begin
   E := Create_Expression ("1 + (2 - 3) * 4");
   Put_Line ("Result: " & To_String (E.Get_Value (Ctx)));
   Start := Clock;
   for I in 1 .. 1_00_000 loop
      E := Create_Expression ("12+232+1+10*100");
   end loop;
   D := Clock - Start;
   D := D * Duration (1_000_000.0 / 100_000.0);
   Put_Line ("Time for parsing using 100000 expressions: 1 parse = " & Duration'Image (D) & "us");

   --  E := Create_Expression ("obj.name");
   Start := Clock;
   for I in 1 .. 100_000 loop
       declare
            Result : constant Object := E.Get_Value (Ctx);
       begin
            null;
       end;
   end loop;
   D := Clock - Start;
   D := D * Duration (1_000_000.0 / 100_000.0);
   Put_Line ("Time for evaluating 100000 expressions: 1 eval = " & Duration'Image (D) & "us");

   declare
      Result : constant Object := E.Get_Value (Ctx);
      S      : constant String := To_String (Result);
   begin
      Put_Line ("Result: " & S);
   end;
end Evaluate;

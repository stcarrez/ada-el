-----------------------------------------------------------------------
--  functions -- Show how to plug and use functions
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Expressions;
with EL.Objects;
with EL.Contexts.Default;
with EL.Functions.Default;
with Ada.Text_IO;
with Bean;
procedure Functions is

   use Bean;
   use Ada.Text_IO;
   use EL.Expressions;
   use EL.Objects;

   E : Expression;

   Fn  : constant EL.Functions.Function_Mapper_Access
     := new EL.Functions.Default.Default_Function_Mapper;
   Ctx : EL.Contexts.Default.Default_Context;

   Joe  : constant Person_Access := Create_Person ("Joe", "Smith", 12);
   Bill : constant Person_Access := Create_Person ("Bill", "Johnson", 42);

   Result : Object;
begin
   --  Register the 'format' function.
   Fn.Set_Function (Namespace => "",
                    Name      => "format",
                    Func      => Bean.Format'Access);
   Ctx.Set_Function_Mapper (Fn);

   --  Create the expression
   E := Create_Expression ("#{format(user.firstName)} #{user.lastName}", Ctx);

   --  Bind the context to 'Joe' and evaluate
   Ctx.Set_Variable ("user", Joe);
   Result := E.Get_Value (Ctx);
   Put_Line ("Joe's name is " & To_String (Result));

   --  Bind the context to 'Bill' and evaluate
   Ctx.Set_Variable ("user", Bill);
   Result := E.Get_Value (Ctx);
   Put_Line ("Bill's name is " & To_String (Result));
end Functions;

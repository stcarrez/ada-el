-----------------------------------------------------------------------
--  EL.Expressions -- Expression Language
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

--
--  First, create the expression object:
--
--    E : constant Expression := EL.Expressions.Create_Expression ("obj.count + 3");
--
--  The expression must be evaluated against an expression context.
--  The expression context will resolve variables whose values can change depending
--  on the evaluation context.  In the example, ''obj'' is the variable name
--  and ''count'' is the property associated with that variable.
--
--    Value : constant Object := E.Get_Value (Context);
--
--  The ''Value'' holds the result which can be a boolean, an integer, a date,
--  a float number or a string.  The value can be converted as follows:
--
--    N : Integer := To_Integer (Value);
--

with EL.Objects;
with Ada.Finalization;

limited with EL.Expressions.Nodes;
with EL.Contexts;
with EL.Beans;
package EL.Expressions is

   use EL.Objects;
   use EL.Contexts;

   --  Exception raised when parsing an invalid expression.
   Invalid_Expression : exception;

   --  Exception raised when a variable cannot be resolved.
   Invalid_Variable : exception;

   --  ------------------------------
   --  Expression
   --  ------------------------------
   type Expression is new Ada.Finalization.Controlled with private;

   --  Get the value of the expression using the given expression context.
   --  Returns an object that holds a typed result.
   function Get_Value (Expr    : Expression;
                       Context : ELContext'Class) return Object;

   --  Parse an expression and return its representation ready for evaluation.
   --  The context is used to resolve the functions.  Variables will be
   --  resolved during evaluation of the expression.
   --  Raises <b>Invalid_Expression</b> if the expression is invalid.
   function Create_Expression (Expr    : String;
                               Context : ELContext'Class)
                               return Expression;

   --  ------------------------------
   --  ValueExpression
   --  ------------------------------   --
   type ValueExpression is new Expression with private;
   type ValueExpression_Access is access all ValueExpression'Class;

   --  Get the value of the expression using the given expression context.
   --  Returns an object that holds a typed result.
   overriding
   function Get_Value (Expr    : in ValueExpression;
                       Context : in ELContext'Class) return Object;

   procedure Set_Value (Expr    : in ValueExpression;
                        Context : in ELContext'Class;
                        Value   : in Object);

   function Is_Readonly (Expr : in ValueExpression) return Boolean;

   --  Parse an expression and return its representation ready for evaluation.
   function Create_Expression (Expr    : String;
                               Context : ELContext'Class)
                               return ValueExpression;

   function Create_ValueExpression (Bean : access EL.Beans.Readonly_Bean'Class)
                                    return ValueExpression;

private

   procedure Adjust   (Object : in out Expression);
   procedure Finalize (Object : in out Expression);

   type Expression is new Ada.Finalization.Controlled with record
      Node : access EL.Expressions.Nodes.ELNode'Class;
   end record;

   type ValueExpression is new Expression with record
      Bean : access EL.Beans.Readonly_Bean'Class;
   end record;

end EL.Expressions;

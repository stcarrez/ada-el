-----------------------------------------------------------------------
--  el-expressions -- Expression Language
--  Copyright (C) 2009, 2010, 2011, 2017, 2020, 2022 Stephane Carrez
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
private with Ada.Strings.Unbounded;

limited private with EL.Expressions.Nodes;
with EL.Contexts;
with Util.Beans.Methods;
package EL.Expressions is

   pragma Preelaborate;

   use EL.Objects;
   use EL.Contexts;

   --  Exception raised when parsing an invalid expression.
   Invalid_Expression : exception;

   --  Exception raised when a variable cannot be resolved.
   Invalid_Variable : exception;

   --  Exception raised when a method cannot be found.
   Invalid_Method : exception;

   --  A parameter is missing in a function call.
   Missing_Argument : exception;

   --  ------------------------------
   --  Expression
   --  ------------------------------
   type Expression is new Ada.Finalization.Controlled with private;
   type Expression_Access is access all Expression'Class;

   --  Check whether the expression is a holds a constant value.
   function Is_Constant (Expr : Expression'Class) return Boolean;

   --  Returns True if the expression is empty (no constant value and no expression).
   function Is_Null (Expr : in Expression'Class) return Boolean;

   --  Get the value of the expression using the given expression context.
   --  Returns an object that holds a typed result.
   function Get_Value (Expr    : Expression;
                       Context : ELContext'Class) return Object;

   --  Get the expression string that was parsed.
   function Get_Expression (Expr : in Expression) return String;

   --  Parse an expression and return its representation ready for evaluation.
   --  The context is used to resolve the functions.  Variables will be
   --  resolved during evaluation of the expression.
   --  Raises <b>Invalid_Expression</b> if the expression is invalid.
   function Create_Expression (Expr    : String;
                               Context : ELContext'Class)
                               return Expression;

   --  Reduce the expression by eliminating known variables and computing
   --  constant expressions.  The result expression is either another
   --  expression or a computed constant value.
   function Reduce_Expression (Expr    : Expression;
                               Context : ELContext'Class)
                               return Expression;

   --  Create an EL expression from an object.
   function Create_Expression (Bean : in EL.Objects.Object)
                               return Expression;

   --  ------------------------------
   --  ValueExpression
   --  ------------------------------   --
   type Value_Expression is new Expression with private;
   type Value_Expression_Access is access all Value_Expression'Class;

   --  Set the value of the expression to the given object value.
   procedure Set_Value (Expr    : in Value_Expression;
                        Context : in ELContext'Class;
                        Value   : in Object);

   --  Returns true if the expression is read-only.
   function Is_Readonly (Expr    : in Value_Expression;
                         Context : in ELContext'Class) return Boolean;

   overriding
   function Reduce_Expression (Expr    : Value_Expression;
                               Context : ELContext'Class)
                               return Value_Expression;

   --  Parse an expression and return its representation ready for evaluation.
   overriding
   function Create_Expression (Expr    : String;
                               Context : ELContext'Class)
                               return Value_Expression;

   function Create_ValueExpression (Bean : EL.Objects.Object)
                                    return Value_Expression;

   --  Create a Value_Expression from an expression.
   --  Raises Invalid_Expression if the expression in not an lvalue.
   function Create_Expression (Expr    : Expression'Class)
                               return Value_Expression;

   --  ------------------------------
   --  Method Expression
   --  ------------------------------
   --  A <b>Method_Expression</b> is an expression that refers to a method.
   --  Information about the object and method is retrieved by using
   --  the <b>Get_Method_Info</b> operation.
   type Method_Expression is new EL.Expressions.Expression with private;

   type Method_Info is record
      Object  : EL.Objects.Object;
      Binding : Util.Beans.Methods.Method_Binding_Access;
   end record;

   --  Evaluate the method expression and return the object and method
   --  binding to execute the method.  The result contains a pointer
   --  to the bean object and a method binding.  The method binding
   --  contains the information to invoke the method
   --  (such as an access to the function or procedure).
   --  Raises the <b>Invalid_Method</b> exception if the method
   --  cannot be resolved.
   function Get_Method_Info (Expr    : Method_Expression;
                             Context : ELContext'Class)
                         return Method_Info;

   --  Parse an expression and return its representation ready for evaluation.
   --  The context is used to resolve the functions.  Variables will be
   --  resolved during evaluation of the expression.
   --  Raises <b>Invalid_Expression</b> if the expression is invalid.
   overriding
   function Create_Expression (Expr    : String;
                               Context : EL.Contexts.ELContext'Class)
                               return Method_Expression;

   --  Reduce the expression by eliminating known variables and computing
   --  constant expressions.  The result expression is either another
   --  expression or a computed constant value.
   overriding
   function Reduce_Expression (Expr    : Method_Expression;
                               Context : EL.Contexts.ELContext'Class)
                               return Method_Expression;

   --  Create a Method_Expression from an expression.
   --  Raises Invalid_Expression if the expression in not an lvalue.
   function Create_Expression (Expr    : Expression'Class)
                               return Method_Expression;

private

   overriding
   procedure Adjust   (Object : in out Expression);

   overriding
   procedure Finalize (Object : in out Expression);

   type Expression is new Ada.Finalization.Controlled with record
      Node  : access EL.Expressions.Nodes.ELNode'Class := null;
      Value : EL.Objects.Object := EL.Objects.Null_Object;
      Expr  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Value_Expression is new Expression with null record;

   type Method_Expression is new EL.Expressions.Expression with null record;

end EL.Expressions;

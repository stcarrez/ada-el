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

with EL.Expressions.Nodes;
with EL.Expressions.Parser;
with Util.Concurrent.Counters;
package body EL.Expressions is

   --  ------------------------------
   --  Check whether the expression is a holds a constant value.
   --  ------------------------------
   function Is_Constant (Expr : Expression'Class) return Boolean is
   begin
      return Expr.Node = null;
   end Is_Constant;

   --  ------------------------------
   --  Get the value of the expression using the given expression context.
   --  ------------------------------
   function Get_Value (Expr    : Expression;
                       Context : ELContext'Class) return Object is
   begin
      if Expr.Node = null then
         return Expr.Value;
      end if;
      return EL.Expressions.Nodes.Get_Value (Expr.Node.all, Context);
   end Get_Value;

   --  ------------------------------
   --  Get the value of the expression using the given expression context.
   --  ------------------------------
   function Get_Value (Expr    : ValueExpression;
                       Context : ELContext'Class) return Object is
   begin
      if Expr.Bean = null then
         if Expr.Node = null then
            return Expr.Value;
         else
            return EL.Expressions.Nodes.Get_Value (Expr.Node.all, Context);
         end if;
      end if;
      return To_Object (Expr.Bean);
   end Get_Value;

   procedure Set_Value (Expr    : in ValueExpression;
                        Context : in ELContext'Class;
                        Value   : in Object) is
   begin
      null;
   end Set_Value;

   function Is_Readonly (Expr : in ValueExpression) return Boolean is
   begin
      if Expr.Bean = null then
         return True;
      end if;
      return not (Expr.Bean.all in EL.Beans.Bean'Class);
   end Is_Readonly;

   --  ------------------------------
   --  Parse an expression and return its representation ready for evaluation.
   --  ------------------------------
   function Create_Expression (Expr    : String;
                               Context : ELContext'Class)
                               return Expression is
      use EL.Expressions.Nodes;
      Result : Expression;
      Node   : EL.Expressions.Nodes.ELNode_Access;
   begin
      EL.Expressions.Parser.Parse (Expr => Expr, Context => Context, Result => Node);
      if Node /= null then
         Result.Node := Node.all'Access;
      end if;
      return Result;
   end Create_Expression;

   --  ------------------------------
   --  Reduce the expression by eliminating known variables and computing
   --  constant expressions.  The result expression is either another
   --  expression or a computed constant value.
   --  ------------------------------
   function Reduce_Expression (Expr    : Expression;
                               Context : ELContext'Class)
                               return Expression is
      use EL.Expressions.Nodes;
      use Ada.Finalization;
   begin
      if Expr.Node = null then
         return Expr;
      end if;
      declare
         Result : constant Reduction := Expr.Node.Reduce (Context);
      begin
         return Expression '(Controlled with
                             Node => Result.Node,
                             Value => Result.Value);
      end;
   end Reduce_Expression;

   function Create_ValueExpression (Bean : access EL.Beans.Readonly_Bean'Class)
                                    return ValueExpression is
      Result : ValueExpression;
   begin
      Result.Bean := Bean;
      return Result;
   end Create_ValueExpression;

   function Create_ValueExpression (Bean : EL.Objects.Object)
                                    return ValueExpression is
      Result : ValueExpression;
   begin
      Result.Value := Bean;
      return Result;
   end Create_ValueExpression;

   --  Parse an expression and return its representation ready for evaluation.
   function Create_Expression (Expr    : String;
                               Context : ELContext'Class)
                               return ValueExpression is
      Result : ValueExpression;
      Node   : EL.Expressions.Nodes.ELNode_Access;
   begin
      EL.Expressions.Parser.Parse (Expr => Expr, Context => Context, Result => Node);
      Result.Node := Node.all'Access;
      return Result;
   end Create_Expression;

   overriding
   function Reduce_Expression (Expr    : ValueExpression;
                               Context : ELContext'Class)
                               return ValueExpression is
      pragma Unreferenced (Context);
   begin
      return Expr;
   end Reduce_Expression;

   procedure Adjust (Object : in out Expression) is
   begin
      if Object.Node /= null then
         Util.Concurrent.Counters.Increment (Object.Node.Ref_Counter);
      end if;
   end Adjust;

   procedure Finalize (Object : in out Expression) is
      Node    : EL.Expressions.Nodes.ELNode_Access;
      Is_Zero : Boolean;
   begin
      if Object.Node /= null then
         Node := Object.Node.all'Access;
         Util.Concurrent.Counters.Decrement (Node.Ref_Counter, Is_Zero);
         if Is_Zero then
            EL.Expressions.Nodes.Delete (Node);
            Object.Node := null;
         end if;
      end if;
   end Finalize;

end EL.Expressions;

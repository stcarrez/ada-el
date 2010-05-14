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
package body EL.Expressions is

   --  ------------------------------
   --  Get the value of the expression using the given expression context.
   --  ------------------------------
   function Get_Value (Expr    : Expression;
                       Context : ELContext'Class) return Object is
   begin
      if Expr.Node = null then
         return EL.Objects.Null_Object;
      end if;
      return EL.Expressions.Nodes.Get_Value (Expr.Node.all, Context);
   end Get_Value;

   --  ------------------------------
   --  Get the value of the expression using the given expression context.
   --  ------------------------------
   function Get_Value (Expr    : ValueExpression;
                       Context : ELContext'Class) return Object is
      pragma Unreferenced (Context);
   begin
      if Expr.Bean = null then
         if Expr.Node = null then
            return EL.Objects.Null_Object;
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

   function Create_ValueExpression (Bean : access EL.Beans.Readonly_Bean'Class)
                                    return ValueExpression is
      Result : ValueExpression;
   begin
      Result.Bean := Bean;
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

   procedure Adjust (Object : in out Expression) is
   begin
      if Object.Node /= null then
         Object.Node.Ref_Counter := Object.Node.Ref_Counter + 1;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Expression) is
      Node : EL.Expressions.Nodes.ELNode_Access;
   begin
      if Object.Node /= null then
         Node := Object.Node.all'Access;
         Node.Ref_Counter := Node.Ref_Counter - 1;
         if Node.Ref_Counter = 0 then
            EL.Expressions.Nodes.Delete (Node);
            Object.Node := null;
         end if;
      end if;
   end Finalize;

end EL.Expressions;

-----------------------------------------------------------------------
--  EL.Expressions -- Expression Nodes
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
with Ada.Unchecked_Deallocation;
with EL.Beans;
with EL.Variables;
package body EL.Expressions.Nodes is

   use EL.Variables;

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   function Get_Value (Expr    : ELUnary;
                       Context : ELContext'Class) return Object is
      Value : constant Object := Expr.Node.Get_Value (Context);
   begin
      case Expr.Kind is
      when EL_NOT =>
         return To_Object (not To_Boolean (Value));

      when EL_MINUS =>
         return -Value;

      when others =>
         return Value;
      end case;
   end Get_Value;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   procedure Delete (Node : in out ELUnary) is
   begin
      Delete (Node.Node);
   end Delete;

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   function Get_Value (Expr    : ELBinary;
                       Context : ELContext'Class) return Object is
      Left  : constant Object := Expr.Left.Get_Value (Context);
      Right : constant Object := Expr.Right.Get_Value (Context);
   begin
      case Expr.Kind is
      when EL_EQ =>
         return To_Object (Left = Right);

      when EL_NE =>
         return To_Object (Left /= Right);

      when EL_LE =>
         return To_Object (Left <= Right);

      when EL_LT =>
         return To_Object (Left < Right);

      when EL_GE =>
         return To_Object (Left >= Right);

      when EL_GT =>
         return To_Object (Left > Right);

      when EL_ADD =>
         return Left + Right;

      when EL_SUB =>
         return Left - Right;

      when EL_MUL =>
         return Left * Right;

      when EL_DIV =>
         return Left / Right;

      when EL_MOD =>
         return Left mod Right;

      when EL_LAND =>
         return To_Object (To_Boolean (Left) and To_Boolean (Right));

      when EL_LOR | EL_OR =>
         return To_Object (To_Boolean (Left) or To_Boolean (Right));

      when EL_AND =>
         return Left & Right;

      end case;
   end Get_Value;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   procedure Delete (Node : in out ELBinary) is
   begin
      Delete (Node.Left);
      Delete (Node.Right);
   end Delete;

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   function Get_Value (Expr    : ELTernary;
                       Context : ELContext'Class) return Object is
      Cond : constant Object := Expr.Cond.Get_Value (Context);
   begin
      if To_Boolean (Cond) then
         return Expr.Left.Get_Value (Context);
      else
         return Expr.Right.Get_Value (Context);
      end if;
   end Get_Value;

   --  Delete the expression tree.  Free the memory allocated by nodes
   --  of the expression tree.  Clears the node pointer.
   procedure Delete (Node : in out ELTernary) is
   begin
      Delete (Node.Right);
      Delete (Node.Left);
      Delete (Node.Cond);
   end Delete;

   --  ------------------------------
   --  Variable to be looked at in the expression context
   --  ------------------------------
   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   function Get_Value (Expr    : ELVariable;
                       Context : ELContext'Class) return Object is
      Mapper   : constant access VariableMapper'Class := Context.Get_Variable_Mapper;
      Resolver : constant ELResolver_Access := Context.Get_Resolver;
   begin
      if Mapper /= null then
         declare
            Value : constant ValueExpression := Mapper.Get_Variable (Expr.Name);
         begin
            return Value.Get_Value (Context);
         end;
      end if;
      if Resolver = null then
         raise Invalid_Variable
           with "Cannot resolve variable: '" & To_String (Expr.Name) & "'";
      end if;
      return Resolver.all.Get_Value (Context, null, Expr.Name);
   exception
      when others =>
         return EL.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   procedure Delete (Node : in out ELVariable) is
   begin
      null;
   end Delete;

   function Get_Value (Expr    : ELValue;
                       Context : ELContext'Class) return Object is

      Var  : constant Object := Expr.Variable.Get_Value (Context);
      Bean : constant access EL.Beans.Readonly_Bean'Class := To_Bean (Var);
   begin
      if Bean /= null then
         return Bean.Get_Value (To_String (Expr.Name));
      else
         return Var;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   procedure Delete (Node : in out ELValue) is
   begin
      Delete (Node.Variable);
   end Delete;

   --  ------------------------------
   --  Literal object (integer, boolean, float, string)
   --  ------------------------------

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   function Get_Value (Expr    : ELObject;
                       Context : ELContext'Class) return Object is
      pragma Unreferenced (Context);
   begin
      return Expr.Value;
   end Get_Value;

   procedure Delete (Node : in out ELObject) is
   begin
      null;
   end Delete;

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   overriding
   function Get_Value (Expr    : ELFunction;
                       Context : ELContext'Class) return Object is
      Arg1, Arg2, Arg3, Arg4 : Object;
   begin
      Arg1 := Expr.Arg1.Get_Value (Context);
      if Expr.Func.Of_Type = F_1_ARG then
         return Expr.Func.Func1 (Arg1);
      end if;
      Arg2 := Expr.Arg2.Get_Value (Context);
      if Expr.Func.Of_Type = F_2_ARG then
         return Expr.Func.Func2 (Arg1, Arg2);
      end if;
      Arg3 := Expr.Arg3.Get_Value (Context);
      if Expr.Func.Of_Type = F_3_ARG then
         return Expr.Func.Func3 (Arg1, Arg2, Arg3);
      end if;
      Arg4 := Expr.Arg4.Get_Value (Context);
      return Expr.Func.Func4 (Arg1, Arg2, Arg3, Arg4);
   end Get_Value;

   overriding
   procedure Delete (Node : in out ELFunction) is
   begin
      if Node.Arg1 /= null then
         Delete (Node.Arg1);
      end if;
      if Node.Arg2 /= null then
         Delete (Node.Arg2);
      end if;
      if Node.Arg3 /= null then
         Delete (Node.Arg3);
      end if;
      if Node.Arg4 /= null then
         Delete (Node.Arg4);
      end if;
   end Delete;

   --  ------------------------------
   --  Create a literal number
   function Create_Node (Value   : Boolean) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => 1);
   end Create_Node;

   --  Create a literal number
   function Create_Node (Value   : Long_Long_Integer) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => 1);
   end Create_Node;

   function Create_Node (Value   : String) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => 1);
   end Create_Node;

   function Create_Node (Value   : Wide_Wide_String) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => 1);
   end Create_Node;

   function Create_Node (Value : Unbounded_Wide_Wide_String) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => 1);
   end Create_Node;

   function Create_Node (Value   : Long_Float) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => 1);
   end Create_Node;

   function Create_Variable (Name : Unbounded_String) return ELNode_Access is
   begin
      return new ELVariable '(Name => Name, Ref_Counter => 1);
   end Create_Variable;

   function Create_Value (Variable : ELNode_Access;
                          Name     : Unbounded_String) return ELNode_Access is
   begin
      return new ELValue '(Name => Name, Variable => Variable, Ref_Counter => 1);
   end Create_Value;

   --  Create unary expressions
   function Create_Node (Of_Type : Unary_Node;
                         Expr    : ELNode_Access) return ELNode_Access is
   begin
      if Expr.all in ELObject'Class then
         case Of_Type is
            when EL_NOT =>
               null;
            when EL_MINUS =>
               null;

            when others =>
               null;
         end case;
      end if;
      return new ELUnary '(Kind => Of_Type, Node => Expr, Ref_Counter => 1);
   end Create_Node;

   --  Create binary expressions
   function Create_Node (Of_Type : Binary_Node;
                         Left    : ELNode_Access;
                         Right   : ELNode_Access) return ELNode_Access is
   begin
      return new ELBinary '(Kind  => Of_Type, Left  => Left, Right => Right,
                            Ref_Counter => 1);
   end Create_Node;

   --  Create a ternary expression.
   function Create_Node (Cond  : ELNode_Access;
                         Left  : ELNode_Access;
                         Right : ELNode_Access) return ELNode_Access is
   begin
      return new ELTernary '(Cond => Cond, Left => Left, Right => Right,
                             Ref_Counter => 1);
   end Create_Node;

   --  Create a function call expression
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => 1,
                              Func        => Func,
                              Arg1        => Arg1,
                              others      => null);
   end Create_Node;

   --  Create a function call expression
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => 1,
                              Func        => Func,
                              Arg1        => Arg1,
                              Arg2        => Arg2,
                              others      => null);
   end Create_Node;

   --  Create a function call expression
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access;
                         Arg3  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => 1,
                              Func        => Func,
                              Arg1        => Arg1,
                              Arg2        => Arg2,
                              Arg3        => Arg3,
                              others      => null);
   end Create_Node;

   --  Create a function call expression
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access;
                         Arg3  : ELNode_Access;
                         Arg4  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => 1,
                              Func        => Func,
                              Arg1        => Arg1,
                              Arg2        => Arg2,
                              Arg3        => Arg3,
                              Arg4        => Arg4);
   end Create_Node;

   --  Delete the expression tree.  Free the memory allocated by nodes
   --  of the expression tree.  Clears the node pointer.
   procedure Delete (Node : in out ELNode_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => ELNode'Class,
                                                        Name   => ELNode_Access);
   begin
      Delete (Node.all);
      Free (Node);
   end Delete;

end EL.Expressions.Nodes;

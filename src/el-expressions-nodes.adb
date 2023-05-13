-----------------------------------------------------------------------
--  el-expressions-nodes -- Expression Nodes
--  Copyright (C) 2009 - 2023 Stephane Carrez
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
with Ada.Characters.Conversions;
with EL.Variables;
with Util.Beans.Methods;
with Util.Beans.Basic;
with Util.Strings;
package body EL.Expressions.Nodes is

   use EL.Variables;
   use Util.Concurrent;

   --  ------------------------------
   --  Evaluate a node on a given context.  If
   --  ------------------------------
   function Get_Safe_Value (Expr    : in ELNode;
                            Context : in ELContext'Class) return Object is
   begin
      return ELNode'Class (Expr).Get_Value (Context);

   exception
      when E : others =>
         Context.Handle_Exception (E);
         return EL.Objects.Null_Object;
   end Get_Safe_Value;

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   overriding
   function Get_Value (Expr    : ELUnary;
                       Context : ELContext'Class) return Object is
   begin
      declare
         Value : constant Object := Expr.Node.Get_Value (Context);
      begin
         case Expr.Kind is
         when EL_NOT =>
            return To_Object (not To_Boolean (Value));

         when EL_MINUS =>
            return -Value;

         when EL_EMPTY =>
            return To_Object (Is_Empty (Value));

         when others =>
            return Value;
         end case;
      end;

   exception
      when E : EL.Variables.No_Variable | EL.Expressions.Invalid_Variable =>
         --  If we can't find the variable, empty predicate must return true.
         if Expr.Kind = EL_EMPTY then
            return To_Object (True);
         end if;

         --  For others, this is an error.
         Context.Handle_Exception (E);
         return EL.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   --  ------------------------------
   overriding
   function Reduce (Expr    : access ELUnary;
                    Context : in ELContext'Class) return Reduction is
      Value : Reduction := Expr.Node.Reduce (Context);
   begin
      if Value.Node /= null then
         Value.Node := Create_Node (Expr.Kind, Value.Node);
      else
         case Expr.Kind is
            when EL_NOT =>
               Value.Value := To_Object (not To_Boolean (Value.Value));

            when EL_MINUS =>
               Value.Value := -Value.Value;

            when EL_EMPTY =>
               Value.Value := To_Object (Is_Empty (Value.Value));

            when others =>
               null;
         end case;
      end if;
      return Value;
   end Reduce;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   overriding
   procedure Delete (Node : in out ELUnary) is
   begin
      Delete (Node.Node);
   end Delete;

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   overriding
   function Get_Value (Expr    : ELBinary;
                       Context : ELContext'Class) return Object is
      Left  : constant Object := Expr.Left.Get_Safe_Value (Context);
      Right : constant Object := Expr.Right.Get_Safe_Value (Context);
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
            return To_Object (To_Boolean (Left) and then To_Boolean (Right));

         when EL_LOR | EL_OR =>
            return To_Object (To_Boolean (Left) or else To_Boolean (Right));

         when EL_CONCAT =>
            --  If one of the object is null, ignore it.
            if Is_Null (Left) then
               return Right;
            end if;
            if Is_Null (Right) then
               return Left;
            end if;
            if Get_Type (Left) = TYPE_WIDE_STRING
              or else Get_Type (Right) = TYPE_WIDE_STRING
            then
               return To_Object (To_Wide_Wide_String (Left)
                                 & To_Wide_Wide_String (Right));
            else
               return To_Object (To_String (Left) & To_String (Right));
            end if;

         when EL_AND =>
            return Left & Right;

      end case;
   end Get_Value;

   --  ------------------------------
   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   --  ------------------------------
   overriding
   function Reduce (Expr    : access ELBinary;
                    Context : in ELContext'Class) return Reduction is
      Left  : Reduction := Expr.Left.Reduce (Context);
      Right : Reduction := Expr.Right.Reduce (Context);
   begin
      --  If at least one value is not constant, return an expression.
      if Left.Node /= null or else Right.Node /= null then
         if Left.Node = null then
            Left.Node := new ELObject '(Value => Left.Value,
                                        Ref_Counter => Counters.ONE);
         elsif Right.Node = null then
            Right.Node := new ELObject '(Value => Right.Value,
                                         Ref_Counter => Counters.ONE);
         end if;
         Left.Node := Create_Node (Expr.Kind, Left.Node, Right.Node);
      else
         --  Both values are known, compute the result.
         case Expr.Kind is
            when EL_EQ =>
               Left.Value := To_Object (Left.Value = Right.Value);

            when EL_NE =>
               Left.Value := To_Object (Left.Value /= Right.Value);

            when EL_LE =>
               Left.Value := To_Object (Left.Value <= Right.Value);

            when EL_LT =>
               Left.Value := To_Object (Left.Value < Right.Value);

            when EL_GE =>
               Left.Value := To_Object (Left.Value >= Right.Value);

            when EL_GT =>
               Left.Value := To_Object (Left.Value > Right.Value);

            when EL_ADD =>
               Left.Value := Left.Value + Right.Value;

            when EL_SUB =>
               Left.Value := Left.Value - Right.Value;

            when EL_MUL =>
               Left.Value := Left.Value * Right.Value;

            when EL_DIV =>
               Left.Value := Left.Value / Right.Value;

            when EL_MOD =>
               Left.Value := Left.Value mod Right.Value;

            when EL_LAND =>
               Left.Value := To_Object (To_Boolean (Left.Value)
                                        and then To_Boolean (Right.Value));

            when EL_LOR | EL_OR =>
               Left.Value := To_Object (To_Boolean (Left.Value)
                                        or else To_Boolean (Right.Value));

            when EL_CONCAT =>
               if Get_Type (Left.Value) = TYPE_WIDE_STRING
                 or else Get_Type (Right.Value) = TYPE_WIDE_STRING
               then
                  Left.Value := To_Object (To_Wide_Wide_String (Left.Value)
                                    & To_Wide_Wide_String (Right.Value));
               else
                  Left.Value := To_Object (To_String (Left.Value)
                                           & To_String (Right.Value));
               end if;

            when EL_AND =>
               Left.Value := Left.Value & Right.Value;

         end case;
      end if;
      return Left;
   end Reduce;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   overriding
   procedure Delete (Node : in out ELBinary) is
   begin
      Delete (Node.Left);
      Delete (Node.Right);
   end Delete;

   --  ------------------------------
   --  Evaluate a node on a given context.
   --  ------------------------------
   overriding
   function Get_Value (Expr    : ELTernary;
                       Context : ELContext'Class) return Object is
      Cond : constant Object := Expr.Cond.Get_Safe_Value (Context);
   begin
      if To_Boolean (Cond) then
         return Expr.Left.Get_Safe_Value (Context);
      else
         return Expr.Right.Get_Safe_Value (Context);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   --  ------------------------------
   overriding
   function Reduce (Expr    : access ELTernary;
                    Context : in ELContext'Class) return Reduction is
      Cond : constant Reduction := Expr.Cond.Reduce (Context);
   begin
      --  Condition value is known, evaluate one or the other part.
      if Cond.Node = null then
         if To_Boolean (Cond.Value) then
            return Expr.Left.Reduce (Context);
         else
            return Expr.Right.Reduce (Context);
         end if;
      end if;
      declare
         Left  : Reduction := Expr.Left.Reduce (Context);
         Right : Reduction := Expr.Right.Reduce (Context);
      begin
         if Left.Node = null then
            Left.Node := new ELObject '(Value => Left.Value,
                                        Ref_Counter => Counters.ONE);
         end if;
         if Right.Node = null then
            Right.Node := new ELObject '(Value => Right.Value,
                                         Ref_Counter => Counters.ONE);
         end if;
         Left.Node := Create_Node (Cond.Node, Left.Node, Right.Node);
         return Left;
      end;
   end Reduce;

   --  ------------------------------
   --  Delete the expression tree.  Free the memory allocated by nodes
   --  of the expression tree.  Clears the node pointer.
   --  ------------------------------
   overriding
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
   overriding
   function Get_Value (Expr    : ELVariable;
                       Context : ELContext'Class) return Object is
      Mapper   : constant access Variable_Mapper'Class := Context.Get_Variable_Mapper;
      Resolver : constant ELResolver_Access := Context.Get_Resolver;
   begin
      --  Resolve using the variable mapper first.  If an exception is raised,
      --  use the context Handle_Exception to give a chance to report custom errors (See ASF).
      --  If the value can't be found and the Handle_Exception did not raised any exception,
      --  return the Null object.
      if Mapper /= null then
         begin
            declare
               Value : constant Expression := Mapper.Get_Variable (Expr.Name);
            begin
               --  If the returned expression is null, assume the variable was not found.
               --  A variable mapper that returns a null expression is faster than raising
               --  the No_Variable exception (around 30us on Intel Core @ 2.6GHz!).
               if not Value.Is_Null then
                  return Value.Get_Value (Context);
               end if;
            end;

         exception
            when No_Variable =>
               if Resolver = null then
                  raise;
               end if;
         end;
      end if;
      if Resolver = null then
         raise Invalid_Variable
           with "Cannot resolve variable: '" & To_String (Expr.Name) & "'";
      end if;
      return Resolver.all.Get_Value (Context, null, Expr.Name);
   end Get_Value;

   --  ------------------------------
   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   --  ------------------------------
   overriding
   function Reduce (Expr    : access ELVariable;
                    Context : in ELContext'Class) return Reduction is
      Mapper   : constant access Variable_Mapper'Class := Context.Get_Variable_Mapper;
   begin
      if Mapper /= null then
         declare
            Value : constant Expression := Mapper.Get_Variable (Expr.Name);
         begin
            if Value.Node /= null then
               return Value.Node.Reduce (Context);

            elsif not EL.Objects.Is_Null (Value.Value) then
               return Reduction '(Value => Value.Value,
                                  Node  => null);

            end if;

         exception
            when others =>
               --  An exception such as Invalid_Variable can be raised if the value expression
               --  defined in <b>Value</b> refers to a variable which is not yet defined.
               --  We want to keep the resolution we did (hence Expr.Name) and still refer
               --  to the new expression so that it can be resolved later on.  Typical case in
               --  ASF:
               --    <h:list value="#{list}" var="item">
               --       <ui:include src="item.xhtml">
               --          <ui:param name="c" value="#{item.components.data}"/>
               --       </ui:include>
               --    </h:list>
               --
               --  Here, the <b>Value</b> will refer to the EL expression #{item.components.data}
               --  which is not known at the time of reduction.
               if Value.Node /= null then
                  Util.Concurrent.Counters.Increment (Value.Node.Ref_Counter);
                  return Reduction '(Value => EL.Objects.Null_Object,
                                     Node  => Value.Node.all'Access);
               end if;
         end;
      end if;
      Util.Concurrent.Counters.Increment (Expr.Ref_Counter);
      return Reduction '(Value => EL.Objects.Null_Object,
                         Node  => Expr.all'Access);

   exception
      when others =>
         Util.Concurrent.Counters.Increment (Expr.Ref_Counter);
         return Reduction '(Value => EL.Objects.Null_Object,
                            Node  => Expr.all'Access);
   end Reduce;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   overriding
   procedure Delete (Node : in out ELVariable) is
   begin
      null;
   end Delete;

   overriding
   function Get_Value (Expr    : ELValue;
                       Context : ELContext'Class) return Object is
      Var  : constant Object := Expr.Variable.Get_Value (Context);
      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class := To_Bean (Var);
   begin
      if Bean /= null then
         return Bean.Get_Value (Expr.Name);
      else
         return Var;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Check if the target bean is a readonly bean.
   --  ------------------------------
   function Is_Readonly (Node    : in ELValue;
                         Context : in ELContext'Class) return Boolean is
      Var  : constant Object := Node.Variable.Get_Value (Context);
      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class := To_Bean (Var);
   begin
      return Bean = null or else not (Bean.all in Util.Beans.Basic.Bean'Class);
   end Is_Readonly;

   --  ------------------------------
   --  Get the variable name.
   --  ------------------------------
   function Get_Variable_Name (Node : in ELValue) return String is
   begin
      if Node.Variable.all in ELVariable'Class then
         return To_String (ELVariable'Class (Node.Variable.all).Name);
      else
         return "?";
      end if;
   end Get_Variable_Name;

   --  ------------------------------
   --  Evaluate the node and return a method info with
   --  the bean object and the method binding.
   --  ------------------------------
   function Get_Method_Info (Node    : in ELValue;
                             Context : in ELContext'Class) return Method_Info is
      use Util.Beans.Methods;
      use type Util.Strings.Name_Access;

      Result : Method_Info;
      Bean   : access Util.Beans.Basic.Readonly_Bean'Class;
   begin
      Result.Object := Node.Variable.Get_Value (Context);
      Bean := To_Bean (Result.Object);
      if Bean = null then
         if EL.Objects.Is_Null (Result.Object) then
            raise Invalid_Variable with "Variable '" & Node.Get_Variable_Name & "' not found";
         else
            raise Invalid_Variable with "Variable '" & Node.Get_Variable_Name & "' has no method";
         end if;
      end if;

      --  If the bean is a method bean, get the methods that it exposes
      --  and look for the binding that matches our method name.
      if Bean.all in Method_Bean'Class then
         declare
            MBean    : constant access Method_Bean'Class := Method_Bean'Class (Bean.all)'Access;
            Bindings : constant Method_Binding_Array_Access := MBean.Get_Method_Bindings;
         begin
            for I in Bindings'Range loop
               if Bindings (I) /= null and then Bindings (I).Name /= null
                 and then Node.Name = Bindings (I).Name.all
               then
                  Result.Binding := Bindings (I);
                  return Result;
               end if;
            end loop;
         end;
      end if;
      raise Invalid_Method with "Method '" & Node.Name & "' not found";
   end Get_Method_Info;

   --  ------------------------------
   --  Evaluate the node and set the value on the associated bean.
   --  Raises Invalid_Variable if the target object is not a bean.
   --  Raises Invalid_Expression if the target bean is not writable.
   --  ------------------------------
   procedure Set_Value (Node    : in ELValue;
                        Context : in ELContext'Class;
                        Value   : in Objects.Object) is
      use Util.Beans;

      Var  : constant Object := Node.Variable.Get_Value (Context);
      Bean : constant access Basic.Readonly_Bean'Class := To_Bean (Var);
   begin
      if Bean = null then
         if EL.Objects.Is_Null (Var) then
            raise Invalid_Variable
              with "Variable '" & Node.Get_Variable_Name & "' not found";
         else
            raise Invalid_Variable
              with "Variable '" & Node.Get_Variable_Name & "' cannot be set";
         end if;
      end if;

      --  If the bean is a method bean, get the methods that it exposes
      --  and look for the binding that matches our method name.
      if not (Bean.all in Basic.Bean'Class) then
         raise Invalid_Method with "Method '" & Node.Name & "' not found";
      end if;
      Basic.Bean'Class (Bean.all).Set_Value (Node.Name, Value);
   end Set_Value;

   --  ------------------------------
   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   --  ------------------------------
   overriding
   function Reduce (Expr    : access ELValue;
                    Context : in ELContext'Class) return Reduction is
      Var : Reduction := Expr.Variable.Reduce (Context);
   begin
      if Var.Node = null then
         declare
            Bean : constant access Util.Beans.Basic.Readonly_Bean'Class
              := To_Bean (Var.Value);
         begin
            if Bean /= null then
               Var.Value := Bean.Get_Value (Expr.Name);
               Var.Node  := null;
               return Var;
            end if;
         end;
      end if;

      --  If the reduction returned the same variable, return the same ELvalue.
      --  Release the counter for the returned variable and increment the other one.
      if Var.Node = Expr.Variable then
         Util.Concurrent.Counters.Decrement (Var.Node.Ref_Counter);
         Util.Concurrent.Counters.Increment (Expr.Ref_Counter);
         return Reduction '(Node  => Expr.all'Access,
                            Value => EL.Objects.Null_Object);
      else
         --  Otherwise, replace the variable.
         return Reduction '(Node => new ELValue '(Variable => Var.Node,
                                                  Len      => Expr.Len,
                                                  Name     => Expr.Name,
                                                  Ref_Counter => Counters.ONE),
                            Value => EL.Objects.Null_Object);
      end if;
   end Reduce;

   --  ------------------------------
   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   --  ------------------------------
   overriding
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
   overriding
   function Get_Value (Expr    : ELObject;
                       Context : ELContext'Class) return Object is
      pragma Unreferenced (Context);
   begin
      return Expr.Value;
   end Get_Value;

   --  ------------------------------
   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   --  ------------------------------
   overriding
   function Reduce (Expr    : access ELObject;
                    Context : in ELContext'Class) return Reduction is
      pragma Unreferenced (Context);
   begin
      return Reduction '(Value => Expr.Value,
                         Node  => null);
   end Reduce;

   overriding
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
      if Expr.Arg1 = null then
         raise Missing_Argument with "Missing argument 1";
      end if;
      Arg1 := Expr.Arg1.Get_Safe_Value (Context);
      if Expr.Func.Of_Type = F_1_ARG then
         return Expr.Func.Func1 (Arg1);
      end if;
      if Expr.Arg2 = null then
         raise Missing_Argument with "Missing argument 2";
      end if;
      Arg2 := Expr.Arg2.Get_Safe_Value (Context);
      if Expr.Func.Of_Type = F_2_ARG then
         return Expr.Func.Func2 (Arg1, Arg2);
      end if;

      if Expr.Arg3 = null then
         raise Missing_Argument with "Missing argument 3";
      end if;
      Arg3 := Expr.Arg3.Get_Safe_Value (Context);
      if Expr.Func.Of_Type = F_3_ARG then
         return Expr.Func.Func3 (Arg1, Arg2, Arg3);
      end if;

      if Expr.Arg4 = null then
         raise Missing_Argument with "Missing argument 4";
      end if;
      Arg4 := Expr.Arg4.Get_Safe_Value (Context);
      return Expr.Func.Func4 (Arg1, Arg2, Arg3, Arg4);
   end Get_Value;

   --  ------------------------------
   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   --  ------------------------------
   overriding
   function Reduce (Expr    : access ELFunction;
                    Context : in ELContext'Class) return Reduction is
      Arg1, Arg2, Arg3, Arg4 : Reduction;
   begin
      if Expr.Arg1 /= null then
         Arg1 := Expr.Arg1.Reduce (Context);
      end if;
      if Expr.Func.Of_Type = F_1_ARG then
         if Arg1.Node = null and then Expr.Func.Optimize
           and then Expr.Func.Func1 /= null
         then
            Arg1.Value := Expr.Func.Func1 (Arg1.Value);
            return Arg1;
         end if;
         if Arg1.Node = null then
            Arg1.Node := new ELObject '(Value       => Arg1.Value,
                                        Ref_Counter => Counters.ONE);
         end if;
         Arg1.Node := Create_Node (Expr.Func, Arg1.Node);
         return Arg1;
      end if;

      if Expr.Arg2 /= null then
         Arg2 := Expr.Arg2.Reduce (Context);
      end if;
      if Expr.Func.Of_Type = F_2_ARG then
         if Arg1.Node = null and then Arg2.Node = null
           and then Expr.Func.Optimize
           and then Expr.Func.Func2 /= null
         then
            Arg1.Value := Expr.Func.Func2 (Arg1.Value, Arg2.Value);
            return Arg1;
         end if;
         if Arg1.Node = null then
            Arg1.Node := new ELObject '(Value       => Arg1.Value,
                                        Ref_Counter => Counters.ONE);
         end if;
         if Arg2.Node = null then
            Arg2.Node := new ELObject '(Value       => Arg2.Value,
                                        Ref_Counter => Counters.ONE);
         end if;
         Arg1.Node := Create_Node (Expr.Func,
                                   Arg1.Node, Arg2.Node);
         return Arg1;
      end if;

      if Expr.Arg3 /= null then
         Arg3 := Expr.Arg3.Reduce (Context);
      end if;
      if Expr.Func.Of_Type = F_3_ARG then
         if Arg1.Node = null and then Arg2.Node = null
           and then Arg3.Node = null
           and then Expr.Func.Optimize
           and then Expr.Func.Func3 /= null
         then
            Arg1.Value := Expr.Func.Func3 (Arg1.Value, Arg2.Value, Arg3.Value);
            return Arg1;
         end if;
         if Arg1.Node = null then
            Arg1.Node := new ELObject '(Value       => Arg1.Value,
                                        Ref_Counter => Counters.ONE);
         end if;
         if Arg2.Node = null then
            Arg2.Node := new ELObject '(Value       => Arg2.Value,
                                        Ref_Counter => Counters.ONE);
         end if;
         if Arg3.Node = null then
            Arg3.Node := new ELObject '(Value       => Arg3.Value,
                                        Ref_Counter => Counters.ONE);
         end if;
         Arg1.Node := Create_Node (Expr.Func,
                                   Arg1.Node, Arg2.Node, Arg3.Node);
         return Arg1;
      end if;

      if Expr.Arg4 /= null then
         Arg4 := Expr.Arg4.Reduce (Context);
      end if;
      if Arg1.Node = null and then Arg2.Node = null and then Arg3.Node = null
        and then Arg4.Node = null and then Expr.Func.Optimize
        and then Expr.Func.Func4 /= null
      then
         Arg1.Value := Expr.Func.Func4 (Arg1.Value, Arg2.Value, Arg3.Value, Arg4.Value);
         return Arg1;
      end if;
      if Arg1.Node = null then
         Arg1.Node := new ELObject '(Value       => Arg1.Value,
                                     Ref_Counter => Counters.ONE);
      end if;
      if Arg2.Node = null then
         Arg2.Node := new ELObject '(Value       => Arg2.Value,
                                     Ref_Counter => Counters.ONE);
      end if;
      if Arg3.Node = null then
         Arg3.Node := new ELObject '(Value       => Arg3.Value,
                                     Ref_Counter => Counters.ONE);
      end if;
      if Arg4.Node = null then
         Arg4.Node := new ELObject '(Value       => Arg4.Value,
                                     Ref_Counter => Counters.ONE);
      end if;
      Arg1.Node := Create_Node (Expr.Func,
                                Arg1.Node, Arg2.Node, Arg3.Node, Arg4.Node);
      return Arg1;
   end Reduce;

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
   --  ------------------------------
   function Create_Node (Value   : Boolean) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => Counters.ONE);
   end Create_Node;

   --  ------------------------------
   --  Create a literal number
   --  ------------------------------
   function Create_Node (Value   : Long_Long_Integer) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => Counters.ONE);
   end Create_Node;

   function Create_Node (Value   : String) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => Counters.ONE);
   end Create_Node;

   function Create_Node (Value   : Wide_Wide_String) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => Counters.ONE);
   end Create_Node;

   function Create_Node (Value : Unbounded_Wide_Wide_String) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => Counters.ONE);
   end Create_Node;

   function Create_Node (Value   : Long_Float) return ELNode_Access is
   begin
      return new ELObject '(Value => To_Object (Value), Ref_Counter => Counters.ONE);
   end Create_Node;

   function Create_Variable (Name : in Wide_Wide_String) return ELNode_Access is
      Result : constant ELVariable_Access := new ELVariable '(Name        => Null_Unbounded_String,
                                                              Ref_Counter => Counters.ONE);
   begin
      Append (Result.Name, Ada.Characters.Conversions.To_String (Name));
      return Result.all'Access;
   end Create_Variable;

   function Create_Value (Variable : in ELNode_Access;
                          Name     : in Wide_Wide_String) return ELNode_Access is
      Result : constant ELValue_Access := new ELValue '(Len         => Name'Length,
                                                        Variable    => Variable,
                                                        Ref_Counter => Counters.ONE,
                                                        Name        => (others => <>));
      Pos    : Positive := 1;
   begin
      for I in Name'Range loop
         Result.Name (Pos) := Ada.Characters.Conversions.To_Character (Name (I));
         Pos := Pos + 1;
      end loop;
      return Result.all'Access;
   end Create_Value;

   --  ------------------------------
   --  Create unary expressions
   --  ------------------------------
   function Create_Node (Of_Type : Unary_Node;
                         Expr    : ELNode_Access) return ELNode_Access is
   begin
      return new ELUnary '(Kind => Of_Type, Node => Expr, Ref_Counter => Counters.ONE);
   end Create_Node;

   --  ------------------------------
   --  Create binary expressions
   --  ------------------------------
   function Create_Node (Of_Type : Binary_Node;
                         Left    : ELNode_Access;
                         Right   : ELNode_Access) return ELNode_Access is
   begin
      return new ELBinary '(Kind  => Of_Type, Left  => Left, Right => Right,
                            Ref_Counter => Counters.ONE);
   end Create_Node;

   --  ------------------------------
   --  Create a ternary expression.
   --  ------------------------------
   function Create_Node (Cond  : ELNode_Access;
                         Left  : ELNode_Access;
                         Right : ELNode_Access) return ELNode_Access is
   begin
      return new ELTernary '(Cond => Cond, Left => Left, Right => Right,
                             Ref_Counter => Counters.ONE);
   end Create_Node;

   --  ------------------------------
   --  Create a function call expression
   --  ------------------------------
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => Counters.ONE,
                              Func        => Func,
                              Arg1        => Arg1,
                              others      => null);
   end Create_Node;

   --  ------------------------------
   --  Create a function call expression
   --  ------------------------------
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => Counters.ONE,
                              Func        => Func,
                              Arg1        => Arg1,
                              Arg2        => Arg2,
                              others      => null);
   end Create_Node;

   --  ------------------------------
   --  Create a function call expression
   --  ------------------------------
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access;
                         Arg3  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => Counters.ONE,
                              Func        => Func,
                              Arg1        => Arg1,
                              Arg2        => Arg2,
                              Arg3        => Arg3,
                              others      => null);
   end Create_Node;

   --  ------------------------------
   --  Create a function call expression
   --  ------------------------------
   function Create_Node (Func  : Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access;
                         Arg3  : ELNode_Access;
                         Arg4  : ELNode_Access) return ELNode_Access is
   begin
      return new ELFunction '(Ref_Counter => Counters.ONE,
                              Func        => Func,
                              Arg1        => Arg1,
                              Arg2        => Arg2,
                              Arg3        => Arg3,
                              Arg4        => Arg4);
   end Create_Node;

   --  ------------------------------
   --  Delete the expression tree.  Free the memory allocated by nodes
   --  of the expression tree.  Clears the node pointer.
   --  ------------------------------
   procedure Delete (Node : in out ELNode_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => ELNode'Class,
                                                        Name   => ELNode_Access);
      Is_Zero : Boolean;
   begin
      if Node /= null then
         Util.Concurrent.Counters.Decrement (Node.Ref_Counter, Is_Zero);
         if Is_Zero then
            Delete (Node.all);
            Free (Node);
         end if;
      end if;
   end Delete;

end EL.Expressions.Nodes;

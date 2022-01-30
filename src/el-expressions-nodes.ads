-----------------------------------------------------------------------
--  el-expressions-nodes -- Expression Nodes
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2020, 2021 Stephane Carrez
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

--  The 'ELNode' describes an expression that can later be evaluated
--  on an expression context.  Expressions are parsed and translated
--  to a read-only tree.

with EL.Functions;
with Ada.Strings.Wide_Wide_Unbounded;
private with Ada.Strings.Unbounded;
with Util.Concurrent.Counters;
private package EL.Expressions.Nodes is

   pragma Preelaborate;

   use EL.Functions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Strings.Unbounded;

   type Reduction;

   type ELNode is abstract tagged limited record
      Ref_Counter : Util.Concurrent.Counters.Counter;
   end record;

   type ELNode_Access is access all ELNode'Class;

   type Reduction is record
      Node  : ELNode_Access;
      Value : EL.Objects.Object;
   end record;

   --  Evaluate a node on a given context.  If
   function Get_Safe_Value (Expr    : in ELNode;
                            Context : in ELContext'Class) return Object;

   --  Evaluate a node on a given context.
   function Get_Value (Expr    : in ELNode;
                       Context : in ELContext'Class) return Object is abstract;

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   function Reduce (Expr    : access ELNode;
                    Context : in ELContext'Class) return Reduction is abstract;

   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   procedure Delete (Node : in out ELNode) is abstract;

   --  Delete the expression tree.  Free the memory allocated by nodes
   --  of the expression tree.  Clears the node pointer.
   procedure Delete (Node : in out ELNode_Access);

   --  ------------------------------
   --  Unary expression node
   --  ------------------------------
   type ELUnary is new ELNode with private;

   type Unary_Node is (EL_VOID, EL_NOT, EL_MINUS, EL_EMPTY);

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELUnary;
                       Context : ELContext'Class) return Object;

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   overriding
   function Reduce (Expr    : access ELUnary;
                    Context : in ELContext'Class) return Reduction;

   overriding
   procedure Delete (Node : in out ELUnary);

   --  ------------------------------
   --  Binary expression node
   --  ------------------------------
   type ELBinary is new ELNode with private;
   type Binary_Node is (EL_EQ, EL_NE, EL_LE, EL_LT, EL_GE, EL_GT,
                        EL_ADD, EL_SUB, EL_MUL, EL_DIV, EL_MOD,
                        EL_AND, EL_OR, EL_LAND, EL_LOR, EL_CONCAT);

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELBinary;
                       Context : ELContext'Class) return Object;

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   overriding
   function Reduce (Expr    : access ELBinary;
                    Context : in ELContext'Class) return Reduction;

   overriding
   procedure Delete (Node : in out ELBinary);

   --  ------------------------------
   --  Ternary expression node
   --  ------------------------------
   type ELTernary is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELTernary;
                       Context : ELContext'Class) return Object;

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   overriding
   function Reduce (Expr    : access ELTernary;
                    Context : in ELContext'Class) return Reduction;

   overriding
   procedure Delete (Node : in out ELTernary);

   --  ------------------------------
   --  Variable to be looked at in the expression context
   --  ------------------------------
   type ELVariable is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELVariable;
                       Context : ELContext'Class) return Object;

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   overriding
   function Reduce (Expr    : access ELVariable;
                    Context : in ELContext'Class) return Reduction;

   overriding
   procedure Delete (Node : in out ELVariable);

   --  ------------------------------
   --  Value property referring to a variable
   --  ------------------------------
   type ELValue (Len : Natural) is new ELNode with private;
   type ELValue_Access is access all ELValue'Class;

   --  Check if the target bean is a readonly bean.
   function Is_Readonly (Node    : in ELValue;
                         Context : in ELContext'Class) return Boolean;

   --  Get the variable name.
   function Get_Variable_Name (Node : in ELValue) return String;

   --  Evaluate the node and return a method info with
   --  the bean object and the method binding.
   function Get_Method_Info (Node    : in ELValue;
                             Context : in ELContext'Class) return Method_Info;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELValue;
                       Context : ELContext'Class) return Object;

   --  Evaluate the node and set the value on the associated bean.
   --  Raises Invalid_Variable if the target object is not a bean.
   --  Raises Invalid_Expression if the target bean is not writable.
   procedure Set_Value (Node    : in ELValue;
                        Context : in ELContext'Class;
                        Value   : in Objects.Object);

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   overriding
   function Reduce (Expr    : access ELValue;
                    Context : in ELContext'Class) return Reduction;

   overriding
   procedure Delete (Node : in out ELValue);

   --  ------------------------------
   --  Literal object (integer, boolean, float, string)
   --  ------------------------------
   type ELObject is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELObject;
                       Context : ELContext'Class) return Object;

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   overriding
   function Reduce (Expr    : access ELObject;
                    Context : in ELContext'Class) return Reduction;

   overriding
   procedure Delete (Node : in out ELObject);

   --  ------------------------------
   --  Function call with up to 4 arguments
   --  ------------------------------
   type ELFunction is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELFunction;
                       Context : ELContext'Class) return Object;

   --  Reduce the expression by eliminating variables which are known
   --  and computing constant expressions.  Returns either a new expression
   --  tree or a constant value.
   overriding
   function Reduce (Expr    : access ELFunction;
                    Context : in ELContext'Class) return Reduction;

   overriding
   procedure Delete (Node : in out ELFunction);

   --  Create constant nodes
   function Create_Node (Value : Boolean) return ELNode_Access;
   function Create_Node (Value : Long_Long_Integer) return ELNode_Access;
   function Create_Node (Value : String) return ELNode_Access;
   function Create_Node (Value : Wide_Wide_String) return ELNode_Access;
   function Create_Node (Value : Long_Float) return ELNode_Access;
   function Create_Node (Value : Unbounded_Wide_Wide_String) return ELNode_Access;
   function Create_Variable (Name : in Wide_Wide_String) return ELNode_Access;
   function Create_Value (Variable : in ELNode_Access;
                          Name     : in Wide_Wide_String) return ELNode_Access;

   --  Create unary expressions
   function Create_Node (Of_Type : Unary_Node;
                         Expr    : ELNode_Access) return ELNode_Access;

   --  Create binary expressions
   function Create_Node (Of_Type : Binary_Node;
                         Left    : ELNode_Access;
                         Right   : ELNode_Access) return ELNode_Access;

   --  Create a ternary expression.
   function Create_Node (Cond  : ELNode_Access;
                         Left  : ELNode_Access;
                         Right : ELNode_Access) return ELNode_Access;

   --  Create a function call
   function Create_Node (Func  : EL.Functions.Function_Access;
                         Arg1  : ELNode_Access) return ELNode_Access;

   --  Create a function call
   function Create_Node (Func  : EL.Functions.Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access) return ELNode_Access;

   --  Create a function call
   function Create_Node (Func  : EL.Functions.Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access;
                         Arg3  : ELNode_Access) return ELNode_Access;

   --  Create a function call
   function Create_Node (Func  : EL.Functions.Function_Access;
                         Arg1  : ELNode_Access;
                         Arg2  : ELNode_Access;
                         Arg3  : ELNode_Access;
                         Arg4  : ELNode_Access) return ELNode_Access;

private

   type ELUnary is new ELNode with record
      Kind : Unary_Node;
      Node : ELNode_Access;
   end record;

   --  ------------------------------
   --  Binary nodes
   --  ------------------------------

   type ELBinary is new ELNode with record
      Kind  : Binary_Node;
      Left  : ELNode_Access;
      Right : ELNode_Access;
   end record;

   --  ------------------------------
   --  Ternary expression
   --  ------------------------------
   type ELTernary is new ELNode with record
      Cond  : ELNode_Access;
      Left  : ELNode_Access;
      Right : ELNode_Access;
   end record;

   --  #{bean.name}   - Bean name, Bean property
   --  #{bean[12]}    - Bean name,

   --  Variable to be looked at in the expression context
   type ELVariable is new ELNode with record
      Name     : Unbounded_String;
   end record;
   type ELVariable_Access is access all ELVariable;

   type ELValue (Len : Natural) is new ELNode with record
      Variable : ELNode_Access;
      Name     : String (1 .. Len);
   end record;

   --  A literal object (integer, boolean, float, String)
   type ELObject is new ELNode with record
      Value : Object;
   end record;

   --  A function call with up to 4 arguments.
   type ELFunction is new ELNode with record
      Func : EL.Functions.Function_Access;
      Arg1 : ELNode_Access;
      Arg2 : ELNode_Access;
      Arg3 : ELNode_Access;
      Arg4 : ELNode_Access;
   end record;

end EL.Expressions.Nodes;

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

--  The 'ELNode' describes an expression that can later be evaluated
--  on an expression context.  Expressions are parsed and translated
--  to a read-only tree.

with EL.Functions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Unbounded;
package EL.Expressions.Nodes is

   use EL.Functions;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Ada.Strings.Unbounded;
   --  type ELNode is abstract tagged private;

   type ELNode is abstract tagged record
      Ref_Counter : Natural := 1;
   end record;

   type ELNode_Access is access all ELNode'Class;

   --  Evaluate a node on a given context.
   function Get_Value (Expr    : ELNode;
                       Context : ELContext'Class) return Object is abstract;

   --  Delete the expression tree (calls Delete (ELNode_Access) recursively).
   procedure Delete (Node : in out ELNode) is abstract;

   --  Delete the expression tree.  Free the memory allocated by nodes
   --  of the expression tree.  Clears the node pointer.
   procedure Delete (Node : in out ELNode_Access);

   --  ------------------------------
   --  Unary expression node
   --  ------------------------------
   type ELUnary is new ELNode with private;

   type Unary_Node is (EL_VOID, EL_NOT, EL_MINUS);

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELUnary;
                       Context : ELContext'Class) return Object;

   procedure Delete (Node : in out ELUnary);

   --  ------------------------------
   --  Binary expression node
   --  ------------------------------
   type ELBinary is new ELNode with private;
   type Binary_Node is (EL_EQ, EL_NE, EL_LE, EL_LT, EL_GE, EL_GT,
                        EL_ADD, EL_SUB, EL_MUL, EL_DIV, EL_MOD,
                        EL_AND, EL_OR, EL_LAND, EL_LOR);

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELBinary;
                       Context : ELContext'Class) return Object;

   procedure Delete (Node : in out ELBinary);

   --  ------------------------------
   --  Ternary expression node
   --  ------------------------------
   type ELTernary is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELTernary;
                       Context : ELContext'Class) return Object;

   procedure Delete (Node : in out ELTernary);

   --  ------------------------------
   --  Variable to be looked at in the expression context
   --  ------------------------------
   type ELVariable is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELVariable;
                       Context : ELContext'Class) return Object;

   procedure Delete (Node : in out ELVariable);

   --  ------------------------------
   --  Value property referring to a variable
   --  ------------------------------
   type ELValue is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELValue;
                       Context : ELContext'Class) return Object;

   procedure Delete (Node : in out ELValue);

   --  ------------------------------
   --  Literal object (integer, boolean, float, string)
   --  ------------------------------
   type ELObject is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELObject;
                       Context : ELContext'Class) return Object;

   procedure Delete (Node : in out ELObject);

   --  ------------------------------
   --  Function call with up to 4 arguments
   --  ------------------------------
   type ELFunction is new ELNode with private;

   --  Evaluate a node on a given context.
   overriding
   function Get_Value (Expr    : ELFunction;
                       Context : ELContext'Class) return Object;

   overriding
   procedure Delete (Node : in out ELFunction);

   --  Create constant nodes
   function Create_Node (Value   : Boolean) return ELNode_Access;
   function Create_Node (Value   : Long_Long_Integer) return ELNode_Access;
   function Create_Node (Value   : String) return ELNode_Access;
   function Create_Node (Value   : Wide_Wide_String) return ELNode_Access;
   function Create_Node (Value   : Long_Float) return ELNode_Access;
   function Create_Node (Value : Unbounded_Wide_Wide_String) return ELNode_Access;
   function Create_Variable (Name : Unbounded_String) return ELNode_Access;
   function Create_Value (Variable : ELNode_Access;
                          Name     : Unbounded_String) return ELNode_Access;

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

   type ELValue is new ELNode with record
      Name     : Unbounded_String;
      Variable : ELNode_Access;
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

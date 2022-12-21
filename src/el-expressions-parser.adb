-----------------------------------------------------------------------
--  el-expressions-parsers -- Parser for Expression Language
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2018, 2019, 2021, 2022 Stephane Carrez
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

with Ada.Characters.Conversions;
with Util.Texts.Builders;
with EL.Functions;
package body EL.Expressions.Parser is

   use Ada.Characters.Conversions;

   use EL.Expressions.Nodes;
   use EL.Functions;

   package Wide_Builder is new Util.Texts.Builders (Element_Type => Wide_Wide_Character,
                                                    Input        => Wide_Wide_String,
                                                    Chunk_Size   => 100);

   function To_Wide_Wide_String (Source : in Wide_Builder.Builder) return Wide_Wide_String
                                 renames Wide_Builder.To_Array;

   use Wide_Builder;

   type Token_Type is (T_EOL,
                       T_LEFT_PARENT,
                       T_RIGHT_PARENT,
                       T_LT, T_LE, T_GT, T_GE, T_NE, T_EQ, T_EMPTY,
                       T_NOT,
                       T_OR, T_AND, T_LOGICAL_AND,
                       T_MINUS, T_PLUS, T_MUL, T_DIV, T_MOD, T_DOT,
                       T_QUESTION, T_COLON, T_COMMA,
                       T_NUMBER, T_LITERAL, T_NAME,
                       T_TRUE, T_FALSE, T_NULL,
                       T_BRACE_END,
                       T_UNKNOWN);

   type Parser (Length : Natural) is limited record
      Pos           : Natural;
      Token_Start   : Natural;
      Token_End     : Natural;
      Value         : Long_Long_Integer;
      Pending_Token : Token_Type := T_EOL;
      Mapper        : Function_Mapper_Access;
      Token         : Wide_Builder.Builder (100);
      Expr          : Wide_Wide_String (1 .. Length);
   end record;

   function Create_Node (Value : in Wide_Builder.Builder) return ELNode_Access;

   procedure Put_Back (P : in out Parser; Token : in Token_Type);

   --  Parse a literal or an expression
   procedure Parse_EL (P      : in out Parser;
                       Result : out ELNode_Access);

   procedure Parse_Choice (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_Or (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_And (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_Equality (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_Compare (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_Math (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_Multiply (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_Unary (P : in out Parser; Result : out ELNode_Access);
   procedure Parse_Function (P         : in out Parser;
                             Namespace : in String;
                             Name      : in String;
                             Result    : out ELNode_Access);

   --  Parse the expression buffer to find the next token.
   procedure Peek (P : in out Parser; Token : out Token_Type);
   procedure Parse_Number (P      : in out Parser;
                           Result : out Long_Long_Integer);

   function Create_Node (Value : in Wide_Builder.Builder) return ELNode_Access is
   begin
      return Create_Node (To_Wide_Wide_String (Value));
   end Create_Node;

   --  #{bean.name}
   --  #{12 + 23}
   --  #{bean.name + bean.name}
   --  #{bean.name == 2 ? 'test' : 'foo'}
   --
   --  expr ::= expr ? expr : expr
   --  expr ::= expr <op> expr
   --  expr ::= <unary> expr
   --  expr ::= ( expr )
   --  expr ::= expr ? expr : expr
   --  expr ::= name . name
   --  expr ::= name
   --  expr ::= <number>
   --  expr ::= <literal>
   --  literal ::= '...' | ".."
   --  number ::= [0-9]+
   --

   --  ------------------------------
   --  Parse a literal or an expression
   --  ------------------------------
   procedure Parse_EL (P      : in out Parser;
                       Result : out ELNode_Access) is
      Literal, Node : ELNode_Access;
      C : Wide_Wide_Character;
   begin
      scan :
      while P.Pos <= P.Expr'Last loop
         C := P.Expr (P.Pos);
         if C = '\' then
            P.Pos := P.Pos + 1;
            exit scan when P.Pos > P.Expr'Last;
            C := P.Expr (P.Pos);
            Append (P.Token, C);
            P.Pos := P.Pos + 1;

         elsif C = '#' or else C = '$' then
            P.Pos := P.Pos + 1;
            if P.Pos > P.Expr'Last then
               raise Invalid_Expression with "Missing '{' to start expression";
            end if;
            C := P.Expr (P.Pos);
            if C /= '{' then
               raise Invalid_Expression with "Missing '{' to start expression";
            end if;

            if Length (P.Token) > 0 then
               if Literal /= null then
                  Literal := Create_Node (EL_CONCAT, Literal,
                                          Create_Node (P.Token));
               else
                  Literal := Create_Node (P.Token);
               end if;
            end if;
            P.Pos := P.Pos + 1;
            Parse_Choice (P, Node);
            if P.Pending_Token /= T_BRACE_END then
               raise Invalid_Expression with "Missing '}' to close expression";
            end if;
            P.Pending_Token := T_EOL;
            Clear (P.Token);
            if Literal /= null then
               Literal := Create_Node (EL_CONCAT, Literal, Node);
            else
               Literal := Node;
            end if;
            Node := null;
         else
            Append (P.Token, C);
            P.Pos := P.Pos + 1;
         end if;
      end loop scan;
      if Length (P.Token) > 0 then
         Node := Create_Node (P.Token);
         if Literal /= null then
            Result := Create_Node (EL_CONCAT, Literal, Node);
         else
            Result := Node;
         end if;
      else
         Result := Literal;
      end if;
      Node    := null;
      Literal := null;

   exception
      when others =>
         Delete (Result);
         Delete (Literal);
         Delete (Node);
         raise;
   end Parse_EL;

   --  ------------------------------
   --  Parse a choice expression, then Or.
   --
   --  choice ::= expr '?' expr ':' choice
   --
   --  Section 1.11: Conditional Operator - A ? B : C
   --  ------------------------------
   procedure Parse_Choice (P      : in out Parser;
                           Result : out ELNode_Access) is
      Left, Right : ELNode_Access;
      Token : Token_Type;
   begin
      Parse_Or (P, Result);
      Peek (P, Token);
      if Token /= T_QUESTION then
         Put_Back (P, Token);
         return;
      end if;
      Parse_Or (P, Left);
      Peek (P, Token);
      if Token /= T_COLON then
         raise Invalid_Expression with "Missing :";
      end if;
      Parse_Choice (P, Right);
      Result := Create_Node (Result, Left, Right);

   exception
      when others =>
         Delete (Result);
         Delete (Left);
         Delete (Right);
         raise;
   end Parse_Choice;

   --  ------------------------------
   --  Parse a logical 'or' expression, then 'and'
   --
   --  or-expr ::= and-expr || and-expr
   --
   --  Section 1.9.1 Binary operator
   --  ------------------------------
   procedure Parse_Or (P      : in out Parser;
                       Result : out ELNode_Access) is
      Token : Token_Type;
      Right : ELNode_Access;
   begin
      Parse_And (P, Result);
      loop
         Peek (P, Token);
         exit when Token /= T_OR;
         Parse_And (P, Right);
         Result := Create_Node (EL_LOR, Result, Right);
         Right := null;
      end loop;
      Put_Back (P, Token);

   exception
      when others =>
         Delete (Result);
         Delete (Right);
         raise;
   end Parse_Or;

   --  ------------------------------
   --  Parse a logical 'and' expression, then 'equality'
   --
   --  and-expr ::= equ-expr && equ-expr
   --
   --  Section 1.9.1 Binary operator
   --  ------------------------------
   procedure Parse_And (P     : in out Parser;
                       Result : out ELNode_Access) is
      Token : Token_Type;
      Right : ELNode_Access;
   begin
      Parse_Equality (P, Result);
      loop
         Peek (P, Token);
         exit when Token /= T_LOGICAL_AND;
         Parse_Equality (P, Right);
         Result := Create_Node (EL_LAND, Result, Right);
         Right := null;
      end loop;
      Put_Back (P, Token);

   exception
      when others =>
         Delete (Result);
         Delete (Right);
         raise;
   end Parse_And;

   --  ------------------------------
   --  Parse an equality '==' 'eq' '!=' 'ne expression, then 'compare'
   --
   --  equ-expr ::= cmp-expr '==' cmp-expr
   --  equ-expr ::= cmp-expr '!=' cmp-expr
   --
   --  Section 1.8.2 Relational Operators
   --  ------------------------------
   procedure Parse_Equality (P      : in out Parser;
                             Result : out ELNode_Access) is
      Token : Token_Type;
      Right : ELNode_Access;
   begin
      Parse_Compare (P, Result);
      loop
         Peek (P, Token);
         exit when Token /= T_EQ and then Token /= T_NE;
         Parse_Equality (P, Right);
         if Token = T_EQ then
            Result := Create_Node (EL_EQ, Result, Right);
         else
            Result := Create_Node (EL_NE, Result, Right);
         end if;
         Right := null;
      end loop;
      Put_Back (P, Token);

   exception
      when others =>
         Delete (Result);
         Delete (Right);
         raise;
   end Parse_Equality;

   --  ------------------------------
   --  Parse a comparison operation then Math
   --  expr ::= expr '<' expr
   --  expr ::= expr '<=' expr
   --  expr ::= expr '>' expr
   --  expr ::= expr '=' expr
   --  expr ::= expr '>=' expr
   --
   --  Section 1.8.1 Relational Operators
   --  ------------------------------
   procedure Parse_Compare (P      : in out Parser;
                            Result : out ELNode_Access) is
      Right : ELNode_Access;
      Token : Token_Type;
   begin
      Parse_Math (P, Result);
      loop
         Peek (P, Token);
         case Token is
            when T_LT =>
               Parse_Math (P, Right);
               Result := Create_Node (EL_LT, Result, Right);

            when T_LE =>
               Parse_Math (P, Right);
               Result := Create_Node (EL_LE, Result, Right);

            when T_GT =>
               Parse_Math (P, Right);
               Result := Create_Node (EL_GT, Result, Right);

            when T_GE =>
               Parse_Math (P, Right);
               Result := Create_Node (EL_GE, Result, Right);

            when others =>
               exit;

         end case;
         Right := null;
      end loop;
      Put_Back (P, Token);

   exception
      when others =>
         Delete (Result);
         Delete (Right);
         raise;
   end Parse_Compare;

   --  ------------------------------
   --  Parse a math expression '+' or '-' then Multiply
   --  expr ::= factor '+' expr
   --  expr ::= factor '-' expr
   --  expr ::= factor '&' expr
   --
   --  Section 1.7.1 Binary operators A (+|-) B
   --  ------------------------------
   procedure Parse_Math (P      : in out Parser;
                         Result : out ELNode_Access) is
      Right : ELNode_Access;
      Token : Token_Type;
   begin
      Parse_Multiply (P, Result);
      loop
         Peek (P, Token);
         case Token is
            when T_PLUS =>
               Parse_Multiply (P, Right);
               Result := Create_Node (EL_ADD, Result, Right);

            when T_MINUS =>
               Parse_Multiply (P, Right);
               Result := Create_Node (EL_SUB, Result, Right);

            when T_AND =>
               Parse_Multiply (P, Right);
               Result := Create_Node (EL_AND, Result, Right);

            when others =>
               exit;

         end case;
         Right := null;
      end loop;
      Put_Back (P, Token);

   exception
      when others =>
         Delete (Result);
         Delete (Right);
         raise;
   end Parse_Math;

   --  ------------------------------
   --  Parse a multiply '*' '/' '%' then Unary
   --  factor ::= term '*' factor
   --  factor ::= term '/' factor
   --  factor ::= term
   --
   --  Section 1.7.1 Binary operators A * B
   --  Section 1.7.2 Binary operators A / B
   --  ------------------------------
   procedure Parse_Multiply (P      : in out Parser;
                             Result : out ELNode_Access) is
      Token : Token_Type;
      Right : ELNode_Access;
   begin
      Parse_Unary (P, Result);
      loop
         Peek (P, Token);
         case Token is
            when T_MUL =>
               Parse_Unary (P, Right);
               Result := Create_Node (EL_MUL, Result, Right);

            when T_DIV =>
               Parse_Unary (P, Right);
               Result := Create_Node (EL_DIV, Result, Right);

            when T_MOD =>
               Parse_Unary (P, Right);
               Result := Create_Node (EL_MOD, Result, Right);

            when others =>
               exit;

         end case;
         Right := null;
      end loop;
      Put_Back (P, Token);

   exception
      when others =>
         Delete (Result);
         Delete (Right);
         raise;
   end Parse_Multiply;

   --  ------------------------------
   --  Parse a unary '!' '-' 'not' 'empty' then value
   --  unary ::= '(' choice ')'
   --  unary ::= ! unary
   --  unary ::= not unary
   --  unary ::= '-' unary
   --  term ::= '(' expr ')'
   --  term ::= literal
   --  term ::= ['-'] number ['.' number [{'e' | 'E'} number]]
   --  term ::= name '.' name
   --  number ::= [0-9]+
   --
   --  Section 1.9.2 Unary not operator
   --  Section 1.10 Empty operator
   --  Section 1.12 Parentheses
   --  Section 1.3 Literals
   --  ------------------------------
   procedure Parse_Unary (P      : in out Parser;
                          Result : out ELNode_Access) is
      Token : Token_Type;
      Node  : ELNode_Access;
   begin
      scan_unary :
      loop
         Peek (P, Token);
         case Token is
            --  Parenthesis expression
            when T_LEFT_PARENT =>
               Parse_Choice (P, Result);
               Peek (P, Token);
               if Token /= T_RIGHT_PARENT then
                  raise Invalid_Expression with "Missing ')' at end of expression";
               end if;
               return;

            when T_NOT =>
               Parse_Unary (P, Node);
               Result := Create_Node (EL_NOT, Node);
               return;

            when T_MINUS =>
               Parse_Unary (P, Node);
               Result := Create_Node (EL_MINUS, Node);
               return;

            when T_EMPTY =>
               Parse_Unary (P, Node);
               Result := Create_Node (EL_EMPTY, Node);
               return;

            when T_NUMBER =>
               Result := Create_Node (P.Value);
               return;
               --
            when T_LITERAL =>
               Result := Create_Node (P.Token);
               return;

            when T_TRUE =>
               Result := Create_Node (True);
               return;

            when T_FALSE =>
               Result := Create_Node (False);
               return;

            when T_NULL =>
               Result := Create_Node (False);
               return;

            when T_NAME =>
               --  name
               --  name.name.name
               --  name[expr]
               --  name.name[expr]
               --  name(expr,...,expr)
               --  name(expr,...,expr).name
               --  name(expr,...,expr)[expr]
               declare
                  C    : Wide_Wide_Character;
               begin
                  if P.Pos <= P.Expr'Last then
                     C := P.Expr (P.Pos);
                  else
                     C := ' ';
                  end if;

                     --  Parse a function call
                  if C = ':' then
                     declare
                        Name : constant String
                          := To_String (P.Expr (P.Token_Start .. P.Token_End));
                     begin
                        P.Pos := P.Pos + 1;
                        Peek (P, Token);
                        if P.Pos <= P.Expr'Last then
                           C := P.Expr (P.Pos);
                        else
                           C := ' ';
                        end if;
                        if Token /= T_NAME or else C /= '(' then
                           raise Invalid_Expression with "Missing function name after ':'";
                        end if;
                        Parse_Function (P, Name, To_String (P.Expr (P.Token_Start .. P.Token_End)),
                                        Result);

                     end;
                     if P.Pos > P.Expr'Last then
                        return;
                     end if;
                     C := P.Expr (P.Pos);
                     if C /= '.' and then C /= '[' then
                        return;
                     end if;
                     if C = '.' then
                        P.Pos := P.Pos + 1;
                        Peek (P, Token);
                        if Token /= T_NAME then
                           raise Invalid_Expression with "Missing name name after '.'";
                        end if;
                     end if;

                  --  Parse a function call
                  elsif C = '(' then
                     Parse_Function (P, "", To_String (P.Expr (P.Token_Start .. P.Token_End)),
                                     Result);
                     if P.Pos > P.Expr'Last then
                        return;
                     end if;
                     C := P.Expr (P.Pos);
                     if C /= '.' and then C /= '[' then
                        return;
                     end if;
                     if C = '.' then
                        P.Pos := P.Pos + 1;
                        Peek (P, Token);
                        if Token /= T_NAME then
                           raise Invalid_Expression with "Missing name name after '.'";
                        end if;
                     end if;

                  elsif C /= '.' then
                     Result := Create_Variable (P.Expr (P.Token_Start .. P.Token_End));
                  else
                     Result := null;
                  end if;

                  if C = '.' then
                     if Result /= null then
                        Result := Create_Value (Variable => Result,
                                                Name => P.Expr (P.Token_Start .. P.Token_End));
                        if P.Pos > P.Expr'Last then
                           C := ' ';
                        else
                           C := P.Expr (P.Pos);
                        end if;
                     else
                        Result := Create_Variable (P.Expr (P.Token_Start .. P.Token_End));
                     end if;

                     --  Parse one or several property extensions
                     while C = '.' loop
                        P.Pos := P.Pos + 1;
                        Peek (P, Token);
                        exit when Token /= T_NAME;
                        Result := Create_Value (Variable => Result,
                                                Name     => P.Expr (P.Token_Start .. P.Token_End));
                        if P.Pos <= P.Expr'Last then
                           C := P.Expr (P.Pos);
                        else
                           C := ' ';
                        end if;
                     end loop;
                  end if;

                  --  Recognize a basic form of array index.
                  if C = '[' then
                     P.Pos := P.Pos + 1;
                     Peek (P, Token);
                     if Token = T_NAME then
                        Result := Create_Value (Variable => Result,
                                                Name     => P.Expr (P.Token_Start .. P.Token_End));
                     elsif Token = T_LITERAL then
                        Result := Create_Value (Variable => Result,
                                                Name     => To_Wide_Wide_String (P.Token));

                     else
                        raise Invalid_Expression with "Missing string in array index []";
                     end if;
                     if P.Pos > P.Expr'Last or else P.Expr (P.Pos) /= ']' then
                        raise Invalid_Expression with "Missing ']' to close array index";
                     end if;
                     P.Pos := P.Pos + 1;
                  end if;
               end;
               return;

            when others =>
               raise Invalid_Expression with "Syntax error in expression";
         end case;
      end loop scan_unary;

   exception
      when others =>
         Delete (Result);
         Delete (Node);
         raise;
   end Parse_Unary;

   --  ------------------------------
   --  Put back a token in the buffer.
   --  ------------------------------
   procedure Put_Back (P : in out Parser; Token : in Token_Type) is
   begin
      P.Pending_Token := Token;
   end Put_Back;

   --  ------------------------------
   --  Parse the expression buffer to find the next token.
   --  ------------------------------
   procedure Peek (P : in out Parser; Token : out Token_Type) is
      C, C1 : Wide_Wide_Character;
   begin
      Token := T_UNKNOWN;
      --  If a token was put back, return it.
      if P.Pending_Token /= T_EOL then
         Token := P.Pending_Token;
         P.Pending_Token := T_EOL;
         return;
      end if;

      --  Skip white spaces
      while P.Pos <= P.Expr'Last loop
         C := P.Expr (P.Pos);
         exit when C /= ' ';
         P.Pos := P.Pos + 1;
      end loop;

      --  Check for end of string.
      if P.Pos > P.Expr'Last then
         Token := T_EOL;
         return;
      end if;

      --  See what we have and continue parsing.
      P.Pos := P.Pos + 1;
      case C is
         --  Literal string using single or double quotes
         --  Collect up to the end of the string and put
         --  the result in the parser token result.
         when ''' | '"' =>
            Clear (P.Token);
            while P.Pos <= P.Expr'Last loop
               C1 := P.Expr (P.Pos);
               P.Pos := P.Pos + 1;
               if C1 = '\' then
                  C1 := P.Expr (P.Pos);
                  P.Pos := P.Pos + 1;
               elsif C1 = C then
                  Token := T_LITERAL;
                  return;
               end if;
               Append (P.Token, C1);
            end loop;
            raise Invalid_Expression with "Missing ' or """;

         --  Number
         when '0' .. '9' =>

            P.Pos := P.Pos - 1;
            Parse_Number (P, P.Value);
            if P.Pos <= P.Expr'Last then
               declare
                  Decimal_Part : Long_Long_Integer := 0;
               begin
                  C := P.Expr (P.Pos);
                  if C = '.' then
                     P.Pos := P.Pos + 1;
                     if P.Pos <= P.Expr'Last then
                        C := P.Expr (P.Pos);
                        if C in '0' .. '9' then
                           Parse_Number (P, Decimal_Part);
                        end if;
                     end if;
                  end if;
               end;
            end if;

            Token := T_NUMBER;
            return;

         --  Parse a name composed of letters or digits.
         when 'a' .. 'z' | 'A' .. 'Z' =>
            P.Token_Start := P.Pos - 1;
            while P.Pos <= P.Expr'Last loop
               C1 := P.Expr (P.Pos);
               exit when not (C1 in 'a' .. 'z' or else C1 in 'A' .. 'Z'
                              or else C1 in '0' .. '9' or else C1 = '_');
               P.Pos := P.Pos + 1;
            end loop;
            P.Token_End := P.Pos - 1;

            --  and empty eq false ge gt le lt ne not null true
            case C is
               when 'a' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "and" then
                     Token := T_LOGICAL_AND;
                     return;
                  end if;

               when 'd' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "div" then
                     Token := T_DIV;
                     return;
                  end if;

               when 'e' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "eq" then
                     Token := T_EQ;
                     return;
                  elsif P.Expr (P.Token_Start .. P.Token_End) = "empty" then
                     Token := T_EMPTY;
                     return;
                  end if;

               when 'f' | 'F' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "false" then
                     Token := T_FALSE;
                     return;
                  end if;

               when 'g' | 'G' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "ge" then
                     Token := T_GE;
                     return;

                  elsif P.Expr (P.Token_Start .. P.Token_End) = "gt" then
                     Token := T_GT;
                     return;
                  end if;

               when 'm' | 'M' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "mod" then
                     Token := T_MOD;
                     return;
                  end if;

               when 'l' | 'L' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "le" then
                     Token := T_LE;
                     return;

                  elsif P.Expr (P.Token_Start .. P.Token_End) = "lt" then
                     Token := T_LT;
                     return;
                  end if;

               when 'n' | 'N' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "not" then
                     Token := T_NOT;
                     return;

                  elsif P.Expr (P.Token_Start .. P.Token_End) = "ne" then
                     Token := T_NE;
                     return;

                  elsif P.Expr (P.Token_Start .. P.Token_End) = "null" then
                     Token := T_NULL;
                     return;
                  end if;

               when 'o' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "or" then
                     Token := T_OR;
                     return;
                  end if;

               when 't' | 'T' =>
                  if P.Expr (P.Token_Start .. P.Token_End) = "true" then
                     Token := T_TRUE;
                     return;
                  end if;

               when others =>
                  null;
            end case;
            Token := T_NAME;
            return;

         when '(' =>
            Token := T_LEFT_PARENT;
            return;

         when ')' =>
            Token := T_RIGHT_PARENT;
            return;

         when '+' =>
            Token := T_PLUS;
            return;

         when '-' =>
            Token := T_MINUS;
            return;

         when '.' =>
            Token := T_DOT;
            return;

         when ',' =>
            Token := T_COMMA;
            return;

         when '*' =>
            Token := T_MUL;
            return;

         when '/' =>
            Token := T_DIV;
            return;

         when '%' =>
            Token := T_MOD;
            return;

         when '?' =>
            Token := T_QUESTION;
            return;

         when ':' =>
            Token := T_COLON;
            return;

         when '!' =>
            Token := T_NOT;
            if P.Pos <= P.Expr'Last then
               C1 := P.Expr (P.Pos);
               if C1 = '=' then
                  P.Pos := P.Pos + 1;
                  Token := T_NE;
               end if;
            end if;
            return;

         when '<' =>
            --  Comparison operators < or <=
            Token := T_LT;
            if P.Pos <= P.Expr'Last then
               C1 := P.Expr (P.Pos);
               if C1 = '=' then
                  P.Pos := P.Pos + 1;
                  Token := T_LE;
               end if;
            end if;
            return;

         when '>' =>
            --  Comparison operators > or >=
            Token := T_GT;
            if P.Pos <= P.Expr'Last then
               C1 := P.Expr (P.Pos);
               if C1 = '=' then
                  P.Pos := P.Pos + 1;
                  Token := T_GE;
               end if;
            end if;
            return;

         when '&' =>
            Token := T_AND;
            if P.Pos <= P.Expr'Last then
               C1 := P.Expr (P.Pos);
               if C1 = '&' then
                  Token := T_LOGICAL_AND;
                  P.Pos := P.Pos + 1;
               end if;
            end if;
            return;

         when '|' =>
            Token := T_UNKNOWN;
            if P.Pos <= P.Expr'Last then
               C1 := P.Expr (P.Pos);
               if C1 = '|' then
                  Token := T_OR;
                  P.Pos := P.Pos + 1;
               end if;
            end if;
            return;

         when '=' =>
            Token := T_UNKNOWN;
            if P.Pos <= P.Expr'Last then
               C1 := P.Expr (P.Pos);
               if C1 = '=' then
                  Token := T_EQ;
                  P.Pos := P.Pos + 1;
               end if;
            end if;
            return;

         when '}' =>
            Token := T_BRACE_END;
            return;

         when others =>
            Token := T_UNKNOWN;
            return;
      end case;
   end Peek;

   --  ------------------------------
   --  Parse a number
   --  ------------------------------
   procedure Parse_Number (P      : in out Parser;
                           Result : out Long_Long_Integer) is
      Value : Long_Long_Integer := 0;
      Num   : Long_Long_Integer;
      C     : Wide_Wide_Character;
   begin
      while P.Pos <= P.Expr'Last loop
         C := P.Expr (P.Pos);
         exit when C not in '0' .. '9';
         Num := Wide_Wide_Character'Pos (C) - Wide_Wide_Character'Pos ('0');
         Value := Value * 10 + Num;
         P.Pos := P.Pos + 1;
      end loop;
      Result := Value;
   end Parse_Number;

   --  ------------------------------
   --  Parse a function call.
   --  The function call can have up to 4 arguments.
   --  ------------------------------
   procedure Parse_Function (P         : in out Parser;
                             Namespace : in String;
                             Name      : in String;
                             Result    : out ELNode_Access) is
      Token : Token_Type;
      Arg1, Arg2, Arg3, Arg4 : ELNode_Access;
      Func : Function_Access;
   begin

      if P.Mapper = null then
         raise Invalid_Expression with "There is no function mapper";
      end if;

      Func := P.Mapper.Get_Function (Namespace, Name);
      --  if Func = null then
      --   raise Invalid_Expression with "Function '" & N & "' not found";
      --  end if;

      --  Extract the first argument.
      --  Number of arguments is pre-defined
      P.Pos := P.Pos + 1;
      Parse_Choice (P, Arg1);
      Peek (P, Token);
      if Token /= T_COMMA then
         if Token /= T_RIGHT_PARENT then
            raise Invalid_Expression with "Missing ')' at end of function call";
         end if;
         Result := Create_Node (Func, Arg1);
         return;
      end if;
      Parse_Choice (P, Arg2);
      Peek (P, Token);
      if Token /= T_COMMA then
         if Token /= T_RIGHT_PARENT then
            raise Invalid_Expression with "Missing ')' at end of function call";
         end if;
         Result := Create_Node (Func, Arg1, Arg2);
         return;
      end if;
      Parse_Choice (P, Arg3);
      Peek (P, Token);
      if Token /= T_COMMA then
         if Token /= T_RIGHT_PARENT then
            raise Invalid_Expression with "Missing ')' at end of function call";
         end if;
         Result := Create_Node (Func, Arg1, Arg2, Arg3);
         return;
      end if;
      Parse_Choice (P, Arg4);
      Peek (P, Token);
      if Token /= T_RIGHT_PARENT then
         raise Invalid_Expression with "Missing ')' at end of function call";
      end if;
      Result := Create_Node (Func, Arg1, Arg2, Arg3, Arg4);

   exception
      when others =>
         Delete (Result);
         Delete (Arg1);
         Delete (Arg2);
         Delete (Arg3);
         Delete (Arg4);
         raise;
   end Parse_Function;

   procedure Parse (Expr    : in String;
                    Context : in ELContext'Class;
                    Result  : out EL.Expressions.Nodes.ELNode_Access) is
      P : Parser (Length => Expr'Length);
   begin
      Result   := null;
      P.Mapper := Context.Get_Function_Mapper;
      P.Expr   := To_Wide_Wide_String (Expr);
      P.Pos    := P.Expr'First;
      Parse_EL (P, Result);
      if P.Pos <= P.Expr'Last or else P.Pending_Token /= T_EOL then
         raise Invalid_Expression with "Syntax error at end of expression";
      end if;

   exception
      when others =>
         Delete (Result);
         raise;
   end Parse;

   procedure Parse (Expr    : in Wide_Wide_String;
                    Context : in ELContext'Class;
                    Result  : out EL.Expressions.Nodes.ELNode_Access) is
      P : Parser (Length => Expr'Length);
   begin
      Result   := null;
      P.Mapper := Context.Get_Function_Mapper;
      P.Expr := Expr;
      P.Pos := P.Expr'First;
      Parse_EL (P, Result);
      if P.Pos <= P.Expr'Last or else P.Pending_Token /= T_EOL then
         raise Invalid_Expression with "Syntax error at end of expression";
      end if;

   exception
      when others =>
         Delete (Result);
         raise;
   end Parse;

end EL.Expressions.Parser;

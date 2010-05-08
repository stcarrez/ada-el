-----------------------------------------------------------------------
--  EL.Objects -- Generic Typed Data Representation
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

--  package EL
--  package EL.Expressions
--  package EL.Objects
with Ada.Characters.Conversions;
with Ada.Calendar.Formatting;
with Ada.Calendar.Conversions;
with Interfaces.C;
package body EL.Objects is

   use Ada.Characters.Conversions;
   use Ada.Calendar.Formatting;
   use type Interfaces.C.long;

   --  Find the data type to be used for an arithmetic operation between two objects.
   function Get_Arithmetic_Type (Left, Right : Object) return Data_Type;

   --  Find the data type to be used for a composition operation between two objects.
   function Get_Compose_Type (Left, Right : Object) return Data_Type;

   --  Find the best type to be used to compare two operands.
   function Get_Compare_Type (Left, Right : Object) return Data_Type;

   function "+"(Str : in String)
                return Unbounded_String renames To_Unbounded_String;

   type Basic_Type is new Object_Type with record
      Name : Unbounded_String;
   end record;

   --  Get the type name
   function Get_Name (Type_Def : Basic_Type) return String;

   --  Translate the object
   function To_String (Type_Def : Basic_Type;
                       Value    : in Object) return String;

   Integer_Type     : aliased constant Basic_Type := Basic_Type '(Name => +("Integer"));
   Boolean_Type     : aliased constant Basic_Type := Basic_Type '(Name => +("Boolean"));
   String_Type      : aliased constant Basic_Type := Basic_Type '(Name => +("String"));
   Wide_String_Type : aliased constant Basic_Type := Basic_Type '(Name => +("Wide_String"));
   Float_Type       : aliased constant Basic_Type := Basic_Type '(Name => +("Float"));
   Time_Type        : aliased constant Basic_Type := Basic_Type '(Name => +("Time"));
   Bean_Type        : aliased constant Basic_Type := Basic_Type '(Name => +("Bean"));

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   function Get_Name (Type_Def : Basic_Type) return String is
   begin
      return To_String (Type_Def.Name);
   end Get_Name;

   --  ------------------------------
   --  Check whether the object contains a value.
   --  Returns true if the object does not contain a value.
   --  ------------------------------
   function Is_Null (Value : in Object) return Boolean is
   begin
      return Value.Of_Type = TYPE_NULL;
   end Is_Null;

   --  ------------------------------
   --  Translate the object
   --  ------------------------------
   function To_String (Type_Def : Basic_Type;
                       Value    : in Object) return String is
      pragma Unreferenced (Type_Def);
   begin
      return To_String (Value);
   end To_String;

   --  ------------------------------
   --  Generic Object holding a value
   --  ------------------------------

   --  Get the type name
   function Get_Type_Name (Value : Object) return String is
   begin
      return Value.Type_Def.Get_Name;
   end Get_Type_Name;

   --  ------------------------------
   --  Get a type identification for the object value.
   --  ------------------------------
   function Get_Type (Value : in Object) return Data_Type is
   begin
      return Value.Of_Type;
   end Get_Type;

   --  ------------------------------
   --  Get the type definition of the object value.
   --  ------------------------------
   function Get_Type (Value : Object) return Object_Type'Class is
   begin
      return Value.Type_Def.all;
   end Get_Type;

   --  ------------------------------
   --  Convert the object to the corresponding type.
   --  ------------------------------
   function To_String (Value : Object) return String is
   begin
      case Value.Of_Type is
         when TYPE_INTEGER =>
            return Long_Long_Integer'Image (Value.Int_Value);

         when TYPE_BOOLEAN =>
            return Boolean'Image (Value.Bool_Value);

         when TYPE_FLOAT =>
            return Long_Long_Float'Image (Value.Float_Value);

         when TYPE_STRING =>
            return To_String (Value.String_Value);

         when TYPE_WIDE_STRING =>
            return To_String (To_Wide_Wide_String (Value.Wide_String_Value));

         when TYPE_TIME =>
            return Ada.Calendar.Formatting.Image (Value.Time_Value);

         when TYPE_EXTERNAL =>
            return Value.Type_Def.To_String (Value);

         when TYPE_BEAN =>
            return "<bean>";

         when TYPE_NULL =>
            return "null";

      end case;
   end To_String;

   --  ------------------------------
   --  Convert the object to a wide string.
   --  ------------------------------
   function To_Wide_Wide_String (Value : Object) return Wide_Wide_String is
   begin
      case Value.Of_Type is
         when TYPE_WIDE_STRING =>
            return To_Wide_Wide_String (Value.Wide_String_Value);

         when TYPE_NULL =>
            return "null";

         when others =>
            return To_Wide_Wide_String (To_String (Value));

      end case;
   end To_Wide_Wide_String;

   --  ------------------------------
   --  Convert the object to an unbounded string.
   --  ------------------------------
   function To_Unbounded_String (Value : Object) return Unbounded_String is
   begin
      case Value.Of_Type is
         when TYPE_STRING =>
            return Value.String_Value;

         when others =>
            return To_Unbounded_String (To_String (Value));

      end case;
   end To_Unbounded_String;

   --  ------------------------------
   --  Convert the object to an unbounded wide string.
   --  ------------------------------
   function To_Unbounded_Wide_Wide_String (Value : Object) return Unbounded_Wide_Wide_String is
   begin
      case Value.Of_Type is
         when TYPE_WIDE_STRING =>
            return Value.Wide_String_Value;

         when TYPE_STRING =>
            return To_Unbounded_Wide_Wide_String
              (To_Wide_Wide_String (To_String (Value.String_Value)));

         when others =>
            return To_Unbounded_Wide_Wide_String (To_Wide_Wide_String (To_String (Value)));

      end case;
   end To_Unbounded_Wide_Wide_String;

   --  ------------------------------
   --  Convert the object to an integer.
   --  ------------------------------
   function To_Integer (Value : Object) return Integer is
   begin
      return Integer (To_Long_Long_Integer (Value));
   end To_Integer;

   --  ------------------------------
   --  Convert the object to an integer.
   --  ------------------------------
   function To_Long_Integer (Value : Object) return Long_Integer is
   begin
      return Long_Integer (To_Long_Long_Integer (Value));
   end To_Long_Integer;

   --  ------------------------------
   --  Convert the object to a long integer.
   --  ------------------------------
   function To_Long_Long_Integer (Value : Object) return Long_Long_Integer is
   begin
      case Value.Of_Type is
         when TYPE_NULL | TYPE_BEAN =>
            return 0;

         when TYPE_INTEGER =>
            return Value.Int_Value;

         when TYPE_BOOLEAN =>
            return Boolean'Pos (Value.Bool_Value);

         when TYPE_FLOAT =>
            return Long_Long_Integer (Value.Float_Value);

         when TYPE_STRING =>
            return Long_Long_Integer'Value (To_String (Value.String_Value));

         when TYPE_TIME =>
            return Long_Long_Integer (Ada.Calendar.Conversions.To_Unix_Time (Value.Time_Value));

         when TYPE_WIDE_STRING =>
            return Long_Long_Integer'Value (To_String (Value));

         when TYPE_EXTERNAL =>
            return Long_Long_Integer'Value (Value.Type_Def.To_String (Value));

      end case;
   end To_Long_Long_Integer;

   --  ------------------------------
   --  Convert the object to an integer.
   --  ------------------------------
   function To_Time (Value : Object) return Ada.Calendar.Time is
      use Interfaces.C;
   begin
      case Value.Of_Type is
         when TYPE_NULL | TYPE_BEAN =>
            return Ada.Calendar.Conversions.To_Ada_Time (0);

         when TYPE_INTEGER =>
            return Ada.Calendar.Conversions.To_Ada_Time (long (Value.Int_Value));

         when TYPE_TIME =>
            return Value.Time_Value;

         when others =>
            return Ada.Calendar.Formatting.Value (To_String (Value));

      end case;
   end To_Time;

   function To_Bean (Value : in Object) return access EL.Beans.Readonly_Bean'Class is
   begin
      if Value.Of_Type = TYPE_BEAN then
         return Value.Bean;
      else
         return null;
      end if;
   end To_Bean;

   --  ------------------------------
   --  Convert the object to a boolean.
   --  ------------------------------
   function To_Boolean (Value : Object) return Boolean is
   begin
      case Value.Of_Type is
         when TYPE_NULL =>
            return False;

         when TYPE_INTEGER =>
            return Value.Int_Value /= 0;

         when TYPE_BOOLEAN =>
            return Value.Bool_Value;

         when TYPE_FLOAT =>
            return Value.Float_Value /= 0.0;

         when TYPE_STRING =>
            return Value.String_Value = "true" or Value.String_Value = "1";

         when TYPE_WIDE_STRING =>
            return Value.Wide_String_Value = "true" or Value.Wide_String_Value = "1";

         when TYPE_TIME =>
            return Ada.Calendar.Conversions.To_Unix_Time (Value.Time_Value) /= 0;

         when TYPE_BEAN =>
            return Value.Bean /= null;

         when TYPE_EXTERNAL =>
            declare
               V : constant String := Value.Type_Def.To_String (Value);
            begin
               return V = "true" or V = "1";
            end;

      end case;
   end To_Boolean;

   --  ------------------------------
   --  Convert the object to a float.
   --  ------------------------------
   function To_Float (Value : Object) return Float is
   begin
      case Value.Of_Type is
         when TYPE_NULL | TYPE_BEAN =>
            return 0.0;

         when TYPE_INTEGER =>
            return Float (Value.Int_Value);

         when TYPE_BOOLEAN =>
            return Float (Boolean'Pos (Value.Bool_Value));

         when TYPE_FLOAT =>
            return Float (Value.Float_Value);

         when TYPE_STRING =>
            return Float'Value (To_String (Value.String_Value));

         when TYPE_WIDE_STRING =>
            return Float'Value (To_String (Value));

         when TYPE_TIME =>
            return Float (Ada.Calendar.Conversions.To_Unix_Time (Value.Time_Value));

         when TYPE_EXTERNAL =>
            return Float'Value (Value.Type_Def.To_String (Value));

      end case;
   end To_Float;

   --  ------------------------------
   --  Convert the object to a long float.
   --  ------------------------------
   function To_Long_Float (Value : Object) return Long_Float is
   begin
      return Long_Float (To_Long_Long_Float (Value));
   end To_Long_Float;

   --  ------------------------------
   --  Convert the object to a long float.
   --  ------------------------------
   function To_Long_Long_Float (Value : Object) return Long_Long_Float is
   begin
      case Value.Of_Type is
         when TYPE_NULL | TYPE_BEAN =>
            return 0.0;

         when TYPE_INTEGER =>
            return Long_Long_Float (Value.Int_Value);

         when TYPE_BOOLEAN =>
            return Long_Long_Float (Boolean'Pos (Value.Bool_Value));

         when TYPE_FLOAT =>
            return Value.Float_Value;

         when TYPE_STRING =>
            return Long_Long_Float'Value (To_String (Value.String_Value));

         when TYPE_WIDE_STRING =>
            return Long_Long_Float'Value (To_String (Value));

         when TYPE_TIME =>
            return Long_Long_Float (Ada.Calendar.Conversions.To_Unix_Time (Value.Time_Value));

         when TYPE_EXTERNAL =>
            return Long_Long_Float'Value (Value.Type_Def.To_String (Value));

      end case;
   end To_Long_Long_Float;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Integer) return Object is
   begin
      return Object '(Of_Type   => TYPE_INTEGER,
                      Int_Value => Long_Long_Integer (Value),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Integer) return Object is
   begin
      return Object '(Of_Type   => TYPE_INTEGER,
                      Int_Value => Long_Long_Integer (Value),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Long_Integer) return Object is
   begin
      return Object '(Of_Type   => TYPE_INTEGER,
                      Int_Value => Value,
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a boolean into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Boolean) return Object is
   begin
      return Object '(Of_Type    => TYPE_BOOLEAN,
                      Bool_Value => Value,
                      Type_Def   => Boolean_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Float) return Object is
   begin
      return Object '(Of_Type     => TYPE_FLOAT,
                      Float_Value => Long_Long_Float (Value),
                      Type_Def    => Float_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a long float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Float) return Object is
   begin
      return Object '(Of_Type     => TYPE_FLOAT,
                      Float_Value => Long_Long_Float (Value),
                      Type_Def    => Float_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a long long float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Long_Float) return Object is
   begin
      return Object '(Of_Type     => TYPE_FLOAT,
                      Float_Value => Value,
                      Type_Def    => Float_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : String) return Object is
   begin
      return Object '(Of_Type => TYPE_STRING,
                      String_Value => +Value,
                      Type_Def => String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a wide string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Wide_Wide_String) return Object is
   begin
      return Object '(Of_Type           => TYPE_WIDE_STRING,
                      Wide_String_Value => To_Unbounded_Wide_Wide_String (Value),
                      Type_Def          => Wide_String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an unbounded string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Unbounded_String) return Object is
   begin
      return Object '(Of_Type      => TYPE_STRING,
                      String_Value => Value,
                      Type_Def     => String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a unbounded wide string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Unbounded_Wide_Wide_String) return Object is
   begin
      return Object '(Of_Type           => TYPE_WIDE_STRING,
                      Wide_String_Value => Value,
                      Type_Def          => Wide_String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a time into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Ada.Calendar.Time) return Object is
   begin
      return Object '(Of_Type    => TYPE_TIME,
                      Time_Value => Value,
                      Type_Def   => Time_Type'Access);
   end To_Object;

   function To_Object (Value : access EL.Beans.Readonly_Bean'Class) return Object is
   begin
      return Object '(Of_Type    => TYPE_BEAN,
                      Bean       => Value,
                      Type_Def   => Bean_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a generic data into a generic typed object.
   --  ------------------------------
   function To_Object (Type_Def : Object_Type_Access;
                       Value    : System.Address) return Object is
   begin
      return Object '(Of_Type  => TYPE_EXTERNAL,
                      Addr     => Value,
                      Type_Def => Type_Def);
   end To_Object;

   --  ------------------------------
   --  Convert the object to an object of another time.
   --  Force the object to be an integer.
   --  ------------------------------
   function Cast_Integer (Value : Object) return Object is
   begin
      return Object '(Of_Type   => TYPE_INTEGER,
                      Int_Value => To_Long_Long_Integer (Value),
                      Type_Def  => Integer_Type'Access);
   end Cast_Integer;

   --  ------------------------------
   --  Force the object to be a float.
   --  ------------------------------
   function Cast_Float (Value : Object) return Object is
   begin
      return Object '(Of_Type     => TYPE_FLOAT,
                      Float_Value => To_Long_Long_Float (Value),
                      Type_Def    => Float_Type'Access);
   end Cast_Float;

   --  ------------------------------
   --  Force the object to be a time.
   --  ------------------------------
   function Cast_Time (Value : Object) return Object is
   begin
      return Object '(Of_Type    => TYPE_TIME,
                      Time_Value => To_Time (Value),
                      Type_Def   => Time_Type'Access);
   end Cast_Time;

   --  ------------------------------
   --  Force the object to be a string.
   --  ------------------------------
   function Cast_String (Value : Object) return Object is
   begin
      return Object '(Of_Type           => TYPE_WIDE_STRING,
                      Wide_String_Value => To_Unbounded_Wide_Wide_String (Value),
                      Type_Def          => Wide_String_Type'Access);
   end Cast_String;

   --  ------------------------------
   --  Find the best type to be used to compare two operands.
   --
   --  ------------------------------
   function Get_Compare_Type (Left, Right : Object) return Data_Type is
   begin
      --  Operands are of the same type.
      if Left.Of_Type = Right.Of_Type then
         return Left.Of_Type;
      end if;

      --  Promote unknown types to a string
      if Right.Of_Type = TYPE_EXTERNAL then
         return Left.Of_Type;
      end if;
      --  12 >= "23"
      --  if Left.Of_Type = TYPE_STRING or
      case Left.Of_Type is
         when TYPE_BOOLEAN =>
            case Right.Of_Type is
               when TYPE_INTEGER | TYPE_BOOLEAN | TYPE_TIME =>
                  return TYPE_INTEGER;

               when TYPE_FLOAT =>
                  return TYPE_FLOAT;

               when TYPE_STRING | TYPE_WIDE_STRING =>
                  null;

               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      if Left.Of_Type = TYPE_BOOLEAN and Right.Of_Type = TYPE_INTEGER then
         return TYPE_INTEGER;
      end if;
      return TYPE_STRING;
   end Get_Compare_Type;

   --  ------------------------------
   --  Find the data type to be used for an arithmetic operation between two objects.
   --  ------------------------------
   function Get_Arithmetic_Type (Left, Right : Object) return Data_Type is
   begin
      if Left.Of_Type = TYPE_FLOAT or Right.Of_Type = TYPE_FLOAT then
         return TYPE_FLOAT;
      end if;
      if Left.Of_Type = TYPE_INTEGER or Right.Of_Type = TYPE_INTEGER then
         return TYPE_INTEGER;
      end if;
      if Left.Of_Type = TYPE_TIME or Right.Of_Type = TYPE_TIME then
         return TYPE_TIME;
      end if;
      if Left.Of_Type = TYPE_BOOLEAN and Right.Of_Type = TYPE_BOOLEAN then
         return TYPE_BOOLEAN;
      end if;
      return TYPE_FLOAT;
   end Get_Arithmetic_Type;

   --  ------------------------------
   --  Find the data type to be used for a composition operation between two objects.
   --  ------------------------------
   function Get_Compose_Type (Left, Right : Object) return Data_Type is
   begin
      if Left.Of_Type = Right.Of_Type then
         return Left.Of_Type;
      end if;
      if Left.Of_Type = TYPE_FLOAT or Right.Of_Type = TYPE_FLOAT then
         return TYPE_FLOAT;
      end if;
      if Left.Of_Type = TYPE_INTEGER or Right.Of_Type = TYPE_INTEGER then
         return TYPE_INTEGER;
      end if;
      if Left.Of_Type = TYPE_TIME or Right.Of_Type = TYPE_TIME then
         return TYPE_TIME;
      end if;
      if Left.Of_Type = TYPE_BOOLEAN and Right.Of_Type = TYPE_BOOLEAN then
         return TYPE_BOOLEAN;
      end if;
      return TYPE_FLOAT;
   end Get_Compose_Type;

   --  ------------------------------
   --  Comparison of objects
   --  ------------------------------
   generic
      with function Int_Comparator (Left, Right : Long_Long_Integer) return Boolean;
      with function Boolean_Comparator (Left, Right : Boolean) return Boolean;
      with function Float_Comparator (Left, Right : Long_Long_Float) return Boolean;
      with function String_Comparator (Left, Right : String) return Boolean;
      with function Wide_String_Comparator (Left, Right : Wide_Wide_String)
                                    return Boolean;
   function Compare (Left, Right : Object) return Boolean;

   --  ------------------------------
   --  Comparison of objects
   --  ------------------------------
   function Compare (Left, Right : Object) return Boolean is
      T : constant Data_Type := Get_Compare_Type (Left, Right);
   begin
      case T is
         when TYPE_BOOLEAN =>
            return Boolean_Comparator (To_Boolean (Left), To_Boolean (Right));

         when TYPE_INTEGER | TYPE_TIME =>
            return Int_Comparator (To_Long_Long_Integer (Left),
                                   To_Long_Long_Integer (Right));

         when TYPE_FLOAT =>
            return Float_Comparator (To_Long_Long_Float (Left),
                                     To_Long_Long_Float (Right));

         when TYPE_STRING =>
            return String_Comparator (To_String (Left), To_String (Right));

         when TYPE_WIDE_STRING =>
            return Wide_String_Comparator (To_Wide_Wide_String (Left),
                                           To_Wide_Wide_String (Right));

         when others =>
            return False;
      end case;
   end Compare;

   function ">" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => ">",
                                   Boolean_Comparator => ">",
                                   Float_Comparator => ">",
                                   String_Comparator => ">",
                                   Wide_String_Comparator => ">");
   begin
      return Cmp (Left, Right);
   end ">";

   function "<" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "<",
                                   Boolean_Comparator => "<",
                                   Float_Comparator => "<",
                                   String_Comparator => "<",
                                   Wide_String_Comparator => "<");
   begin
      return Cmp (Left, Right);
   end "<";

   function "<=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "<=",
                                   Boolean_Comparator => "<=",
                                   Float_Comparator => "<=",
                                   String_Comparator => "<=",
                                   Wide_String_Comparator => "<=");
   begin
      return Cmp (Left, Right);
   end "<=";

   function ">=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => ">=",
                                   Boolean_Comparator => ">=",
                                   Float_Comparator => ">=",
                                   String_Comparator => ">=",
                                   Wide_String_Comparator => ">=");
   begin
      return Cmp (Left, Right);
   end ">=";
--   function "=" (Left, Right : Object) return Boolean;

   function "=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "=",
                                   Boolean_Comparator => "=",
                                   Float_Comparator => "=",
                                   String_Comparator => "=",
                                   Wide_String_Comparator => "=");
   begin
      return Cmp (Left, Right);
   end "=";

   --  ------------------------------
   --  Arithmetic operations of objects
   --  ------------------------------
   generic
      with function Int_Operation (Left, Right : Long_Long_Integer)
                                    return Long_Long_Integer;
      with function Float_Operation (Left, Right : Long_Long_Float)
                                    return Long_Long_Float;
   function Arith (Left, Right : Object) return Object;

   --  Comparison of objects
   function Arith (Left, Right : Object) return Object is
      T : constant Data_Type := Get_Arithmetic_Type (Left, Right);
   begin
      case T is
         when TYPE_INTEGER | TYPE_TIME =>
            return To_Object (Int_Operation (To_Long_Long_Integer (Left),
                                             To_Long_Long_Integer (Right)));

         when TYPE_FLOAT =>
            return To_Object (Float_Operation (To_Long_Long_Float (Left),
                                               To_Long_Long_Float (Right)));

         when others =>
            return Left;
      end case;
   end Arith;

   --  Arithmetic operations on objects
   function "+" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "+",
                                       Float_Operation => "+");
   begin
      return Operation (Left, Right);
   end "+";

   function "-" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "-",
                                       Float_Operation => "-");
   begin
      return Operation (Left, Right);
   end "-";

   function "-" (Left : Object) return Object is
   begin
      case Left.Of_Type is
         when TYPE_INTEGER | TYPE_TIME =>
            return To_Object (-To_Long_Long_Integer (Left));

         when TYPE_FLOAT =>
            return To_Object (-(To_Long_Long_Float (Left)));

         when others =>
            return Left;

      end case;
   end "-";

   function "*" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "*",
                                       Float_Operation => "*");
   begin
      return Operation (Left, Right);
   end "*";

   function "/" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "/",
                                       Float_Operation => "/");
   begin
      return Operation (Left, Right);
   end "/";

   function "mod" (Left, Right : Object) return Object is
      function "mod" (Left, Right : Long_Long_Float) return Long_Long_Float;

      function "mod" (Left, Right : Long_Long_Float) return Long_Long_Float is
         L : constant Long_Long_Integer := Long_Long_Integer (Left);
         R : constant Long_Long_Integer := Long_Long_Integer (Right);
      begin
         return Long_Long_Float (L mod R);
      end "mod";

      function Operation is new Arith (Int_Operation => "mod",
                                       Float_Operation => "mod");
   begin
      return Operation (Left, Right);
   end "mod";

   function "&" (Left, Right : Object) return Object is
      T : constant Data_Type := Get_Compose_Type (Left, Right);
   begin
      case T is
         when TYPE_BOOLEAN =>
            return To_Object (To_Boolean (Left) and To_Boolean (Right));

         when others =>
            return To_Object (To_String (Left) & To_String (Right));

      end case;
   end "&";

end EL.Objects;

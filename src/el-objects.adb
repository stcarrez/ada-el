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
with Ada.Unchecked_Deallocation;
--  with Ada.Calendar.Formatting;
--  with Ada.Calendar.Conversions;
with Interfaces.C;
with EL.Beans;
package body EL.Objects is

   use Util.Concurrent.Counters;
   use Ada.Characters.Conversions;
--     use Ada.Calendar.Formatting;
   use type Interfaces.C.long;

   type String_Access is access constant String;

   --  Find the data type to be used for an arithmetic operation between two objects.
   function Get_Arithmetic_Type (Left, Right : Object) return Data_Type;

   --  Find the data type to be used for a composition operation between two objects.
   function Get_Compose_Type (Left, Right : Object) return Data_Type;

   --  Find the best type to be used to compare two operands.
   function Get_Compare_Type (Left, Right : Object) return Data_Type;

   type Basic_Type is new Object_Type with record
      Name : String_Access;
   end record;

   --  Get the type name
   function Get_Name (Type_Def : Basic_Type) return String;

   INTEGER_NAME     : aliased constant String := "Integer";
   BOOLEAN_NAME     : aliased constant String := "Boolean";
   STRING_NAME      : aliased constant String := "String";
   WIDE_STRING_NAME : aliased constant String := "Wide_Wide_String";
   FLOAT_NAME       : aliased constant String := "Float";
   TIME_NAME        : aliased constant String := "Time";
   BEAN_NAME        : aliased constant String := "Bean";

   Integer_Type     : aliased constant Basic_Type := Basic_Type '(Name => INTEGER_NAME'Access);
   Boolean_Type     : aliased constant Basic_Type := Basic_Type '(Name => BOOLEAN_NAME'Access);
   String_Type      : aliased constant Basic_Type := Basic_Type '(Name => STRING_NAME'Access);
   Wide_String_Type : aliased constant Basic_Type := Basic_Type '(Name => WIDE_STRING_NAME'Access);
   Float_Type       : aliased constant Basic_Type := Basic_Type '(Name => FLOAT_NAME'Access);
   Time_Type        : aliased constant Basic_Type := Basic_Type '(Name => TIME_NAME'Access);
   Bean_Type        : aliased constant Basic_Type := Basic_Type '(Name => BEAN_NAME'Access);

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   function Get_Name (Type_Def : Basic_Type) return String is
   begin
      return Type_Def.Name.all;
   end Get_Name;

   --  ------------------------------
   --  Check whether the object contains a value.
   --  Returns true if the object does not contain a value.
   --  ------------------------------
   function Is_Null (Value : in Object) return Boolean is
   begin
      return Value.V.Of_Type = TYPE_NULL;
   end Is_Null;

   --  ------------------------------
   --  Check whether the object is empty.
   --  If the object is null, returns true.
   --  If the object is the empty string, returns true.
   --  If the object is a list bean whose Get_Count is 0, returns true.
   --  Otherwise returns false.
   --  ------------------------------
   function Is_Empty (Value : in Object) return Boolean is
   begin
      case Value.V.Of_Type is
         when TYPE_NULL =>
            return True;

         when TYPE_STRING =>
            return Value.V.Proxy = null or else Value.V.Proxy.String_Value.all = "";

         when TYPE_WIDE_STRING  =>
            return Value.V.Proxy = null or else Value.V.Proxy.Wide_String_Value.all = "";

         when TYPE_BEAN =>
            if Value.V.Proxy = null or else Value.V.Proxy.Bean = null then
               return True;
            end if;
            if not (Value.V.Proxy.Bean.all in EL.Beans.List_Bean'Class) then
               return False;
            end if;
            declare
               L : constant EL.Beans.List_Bean_Access :=
                 EL.Beans.List_Bean'Class (Value.V.Proxy.Bean.all)'Unchecked_Access;
            begin
               return L.Get_Count = 0;
            end;

         when others =>
            return False;

      end case;
   end Is_Empty;

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
      return Value.V.Of_Type;
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
      case Value.V.Of_Type is
         when TYPE_INTEGER =>
            if Value.V.Int_Value >= 0 then
               declare
                  S : constant String := Long_Long_Integer'Image (Value.V.Int_Value);
               begin
                  return S (S'First + 1 .. S'Last);
               end;
            else
               return Long_Long_Integer'Image (Value.V.Int_Value);
            end if;

         when TYPE_BOOLEAN =>
            return Boolean'Image (Value.V.Bool_Value);

         when TYPE_FLOAT =>
            return Long_Long_Float'Image (Value.V.Float_Value);

         when TYPE_STRING =>
            if Value.V.Proxy = null then
               return "null";
            end if;
            return Value.V.Proxy.String_Value.all;

         when TYPE_WIDE_STRING =>
            if Value.V.Proxy = null then
               return "null";
            end if;
            return To_String (Value.V.Proxy.Wide_String_Value.all);

         when TYPE_TIME =>
            --              return Ada.Calendar.Formatting.Image (Value.V.Time_Value);
            return Long_Long_Integer'Image (Value.V.Time_Value);

         when TYPE_BEAN =>
            if Value.V.Proxy = null or else Value.V.Proxy.Bean = null then
               return "null";
            else
               return "<bean>";
            end if;

         when TYPE_NULL =>
            return "null";

      end case;
   end To_String;

   --  ------------------------------
   --  Convert the object to a wide string.
   --  ------------------------------
   function To_Wide_Wide_String (Value : Object) return Wide_Wide_String is
   begin
      case Value.V.Of_Type is
         when TYPE_WIDE_STRING =>
            if Value.V.Proxy = null then
               return "null";
            end if;
            return Value.V.Proxy.Wide_String_Value.all;

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
      case Value.V.Of_Type is
         when TYPE_STRING =>
            if Value.V.Proxy = null then
               return To_Unbounded_String ("null");
            end if;
            return To_Unbounded_String (Value.V.Proxy.String_Value.all);

         when others =>
            return To_Unbounded_String (To_String (Value));

      end case;
   end To_Unbounded_String;

   --  ------------------------------
   --  Convert the object to an unbounded wide string.
   --  ------------------------------
   function To_Unbounded_Wide_Wide_String (Value : Object) return Unbounded_Wide_Wide_String is
   begin
      case Value.V.Of_Type is
         when TYPE_WIDE_STRING =>
            if Value.V.Proxy = null then
               return To_Unbounded_Wide_Wide_String ("null");
            end if;
            return To_Unbounded_Wide_Wide_String (Value.V.Proxy.Wide_String_Value.all);

         when TYPE_STRING =>
            if Value.V.Proxy = null then
               return To_Unbounded_Wide_Wide_String ("null");
            end if;
            return To_Unbounded_Wide_Wide_String
              (To_Wide_Wide_String (Value.V.Proxy.String_Value.all));

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
      case Value.V.Of_Type is
         when TYPE_NULL | TYPE_BEAN =>
            return 0;

         when TYPE_INTEGER =>
            return Value.V.Int_Value;

         when TYPE_BOOLEAN =>
            return Boolean'Pos (Value.V.Bool_Value);

         when TYPE_FLOAT =>
            return Long_Long_Integer (Value.V.Float_Value);

         when TYPE_STRING =>
            if Value.V.Proxy = null then
               return 0;
            end if;
            return Long_Long_Integer'Value (Value.V.Proxy.String_Value.all);

         when TYPE_TIME =>
            --              return Long_Long_Integer (Ada.Calendar.Conversions.To_Unix_Time (Value.V.Time_Value));
            return Value.V.Time_Value;

         when TYPE_WIDE_STRING =>
            if Value.V.Proxy = null then
               return 0;
            end if;
            return Long_Long_Integer'Value (To_String (Value.V.Proxy.Wide_String_Value.all));

      end case;

   exception
      when Constraint_Error =>
         return 0;
   end To_Long_Long_Integer;

   --  ------------------------------
   --  Convert the object to an integer.
   --  ------------------------------
--     function To_Time (Value : Object) return Ada.Calendar.Time is
--        use Interfaces.C;
--     begin
--        case Value.V.Of_Type is
--           when TYPE_NULL | TYPE_BEAN =>
--              return Ada.Calendar.Conversions.To_Ada_Time (0);
--
--           when TYPE_INTEGER =>
--              return Ada.Calendar.Conversions.To_Ada_Time (long (Value.V.Int_Value));
--
--           when TYPE_TIME =>
--              return Value.V.Time_Value;
--
--           when others =>
--              return Ada.Calendar.Formatting.Value (To_String (Value));
--
--        end case;
--     end To_Time;

   function To_Bean (Value : in Object) return access EL.Beans.Readonly_Bean'Class is
   begin
      if Value.V.Of_Type = TYPE_BEAN and then Value.V.Proxy /= null then
         return Value.V.Proxy.Bean;
      else
         return null;
      end if;
   end To_Bean;

   --  ------------------------------
   --  Convert the object to a boolean.
   --  ------------------------------
   function To_Boolean (Value : Object) return Boolean is
   begin
      case Value.V.Of_Type is
         when TYPE_NULL =>
            return False;

         when TYPE_INTEGER =>
            return Value.V.Int_Value /= 0;

         when TYPE_BOOLEAN =>
            return Value.V.Bool_Value;

         when TYPE_FLOAT =>
            return Value.V.Float_Value /= 0.0;

         when TYPE_STRING =>
            return Value.V.Proxy /= null
              and then (Value.V.Proxy.String_Value.all = "true"
                        or Value.V.Proxy.String_Value.all = "1");

         when TYPE_WIDE_STRING =>
            return Value.V.Proxy /= null
              and then (Value.V.Proxy.Wide_String_Value.all = "true"
                        or Value.V.Proxy.Wide_String_Value.all = "1");

         when TYPE_TIME =>
            --              return Ada.Calendar.Conversions.To_Unix_Time (Value.V.Time_Value) /= 0;
            return Value.V.Time_Value /= 0;

         when TYPE_BEAN =>
            return Value.V.Proxy /= null and then Value.V.Proxy.Bean /= null;

      end case;
   end To_Boolean;

   --  ------------------------------
   --  Convert the object to a float.
   --  ------------------------------
   function To_Float (Value : Object) return Float is
   begin
      case Value.V.Of_Type is
         when TYPE_NULL | TYPE_BEAN =>
            return 0.0;

         when TYPE_INTEGER =>
            return Float (Value.V.Int_Value);

         when TYPE_BOOLEAN =>
            return Float (Boolean'Pos (Value.V.Bool_Value));

         when TYPE_FLOAT =>
            return Float (Value.V.Float_Value);

         when TYPE_STRING =>
            if Value.V.Proxy = null then
               return 0.0;
            end if;
            return Float'Value (Value.V.Proxy.String_Value.all);

         when TYPE_WIDE_STRING =>
            if Value.V.Proxy = null then
               return 0.0;
            end if;
            return Float'Value (To_String (Value.V.Proxy.Wide_String_Value.all));

         when TYPE_TIME =>
            --              return Float (Ada.Calendar.Conversions.To_Unix_Time (Value.V.Time_Value));
            return Float (Value.V.Time_Value);

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
      case Value.V.Of_Type is
         when TYPE_NULL | TYPE_BEAN =>
            return 0.0;

         when TYPE_INTEGER =>
            return Long_Long_Float (Value.V.Int_Value);

         when TYPE_BOOLEAN =>
            return Long_Long_Float (Boolean'Pos (Value.V.Bool_Value));

         when TYPE_FLOAT =>
            return Value.V.Float_Value;

         when TYPE_STRING =>
            if Value.V.Proxy = null then
               return 0.0;
            end if;
            return Long_Long_Float'Value (Value.V.Proxy.String_Value.all);

         when TYPE_WIDE_STRING =>
            return Long_Long_Float'Value (To_String (Value));

         when TYPE_TIME =>
            --              return Long_Long_Float (Ada.Calendar.Conversions.To_Unix_Time (Value.V.Time_Value));
            return Long_Long_Float (Value.V.Time_Value);

      end case;
   end To_Long_Long_Float;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Integer) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => Long_Long_Integer (Value)),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Integer) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => Long_Long_Integer (Value)),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Long_Integer) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => Value),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a boolean into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Boolean) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type    => TYPE_BOOLEAN,
                                          Bool_Value => Value),
                      Type_Def   => Boolean_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Float) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => Long_Long_Float (Value)),
                      Type_Def    => Float_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a long float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Float) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => Long_Long_Float (Value)),
                      Type_Def    => Float_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a long long float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Long_Float) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => Value),
                      Type_Def    => Float_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : String) return Object is
      S : constant Ada.Strings.Unbounded.String_Access := new String '(Value);
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type => TYPE_STRING,
                                          Proxy   => new Bean_Proxy '(Ref_Counter  => ONE,
                                                                      Of_Type      => TYPE_STRING,
                                                                      String_Value => S)),
                      Type_Def     => String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a wide string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Wide_Wide_String) return Object is
      S : constant Wide_Wide_String_Access := new Wide_Wide_String '(Value);
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type => TYPE_WIDE_STRING,
                                          Proxy   => new Bean_Proxy '(Ref_Counter => ONE,
                                                                      Of_Type => TYPE_WIDE_STRING,
                                                                      Wide_String_Value => S)),
                      Type_Def          => Wide_String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an unbounded string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Unbounded_String) return Object is
      S : constant Ada.Strings.Unbounded.String_Access := new String '(To_String (Value));
   begin
      return Object '(Controlled with
        V => Object_Value '(Of_Type => TYPE_STRING,
                            Proxy   => new Bean_Proxy '(Ref_Counter  => ONE,
                                                        Of_Type      => TYPE_STRING,
                                                        String_Value => S)),
        Type_Def     => String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a unbounded wide string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Unbounded_Wide_Wide_String) return Object is
      S : constant Wide_Wide_String_Access := new Wide_Wide_String '(To_Wide_Wide_String (Value));
   begin
      return Object '(Controlled with
        V => Object_Value '(Of_Type => TYPE_WIDE_STRING,
                            Proxy   => new Bean_Proxy '(Ref_Counter => ONE,
                                                        Of_Type => TYPE_WIDE_STRING,
                                                        Wide_String_Value => S)),
        Type_Def          => Wide_String_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a time into a generic typed object.
   --  ------------------------------
--     function To_Object (Value : Ada.Calendar.Time) return Object is
--     begin
--        return Object '(Controlled with
--                        Of_Type    => TYPE_TIME,
--                        Time_Value => Value,
--                        Type_Def   => Time_Type'Access);
--     end To_Object;

   function To_Object (Value : access EL.Beans.Readonly_Bean'Class) return Object is
   begin
      if Value = null then
         return Object '(Controlled with
                         V => Object_Value '(Of_Type    => TYPE_BEAN,
                                             Proxy      => null),
                         Type_Def   => Bean_Type'Access);
      else
         return Object '(Controlled with
                         V => Object_Value '(Of_Type => TYPE_BEAN,
                                             Proxy   => new Bean_Proxy '(Of_Type => TYPE_BEAN,
                                                                         Ref_Counter => ONE,
                                                                         Bean => Value)),
                         Type_Def   => Bean_Type'Access);
      end if;
   end To_Object;

   --  ------------------------------
   --  Convert the object to an object of another time.
   --  Force the object to be an integer.
   --  ------------------------------
   function Cast_Integer (Value : Object) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => To_Long_Long_Integer (Value)),
                      Type_Def  => Integer_Type'Access);
   end Cast_Integer;

   --  ------------------------------
   --  Force the object to be a float.
   --  ------------------------------
   function Cast_Float (Value : Object) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => To_Long_Long_Float (Value)),
                      Type_Def    => Float_Type'Access);
   end Cast_Float;

   --  ------------------------------
   --  Force the object to be a time.
   --  ------------------------------
   function Cast_Time (Value : Object) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type    => TYPE_TIME,
                                          Time_Value => To_Long_Long_Integer (Value)),
                      Type_Def   => Time_Type'Access);
   end Cast_Time;

   --  ------------------------------
   --  Force the object to be a string.
   --  ------------------------------
   function Cast_String (Value : Object) return Object is
   begin
      if Value.V.Of_Type = TYPE_STRING or Value.V.Of_Type = TYPE_WIDE_STRING then
         return Value;
      end if;
      return To_Object (To_Wide_Wide_String (Value));
   end Cast_String;

   --  ------------------------------
   --  Find the best type to be used to compare two operands.
   --
   --  ------------------------------
   function Get_Compare_Type (Left, Right : Object) return Data_Type is
   begin
      --  Operands are of the same type.
      if Left.V.Of_Type = Right.V.Of_Type then
         return Left.V.Of_Type;
      end if;

      --  12 >= "23"
      --  if Left.Of_Type = TYPE_STRING or
      case Left.V.Of_Type is
         when TYPE_BOOLEAN =>
            case Right.V.Of_Type is
               when TYPE_INTEGER | TYPE_BOOLEAN | TYPE_TIME =>
                  return TYPE_INTEGER;

               when TYPE_FLOAT | TYPE_STRING | TYPE_WIDE_STRING =>
                  return Right.V.Of_Type;

               when others =>
                  null;
            end case;

         when TYPE_INTEGER =>
            case Right.V.Of_Type is
               when TYPE_BOOLEAN | TYPE_TIME =>
                  return TYPE_INTEGER;

               when TYPE_FLOAT =>
                  return TYPE_FLOAT;

               when others =>
                  null;
            end case;

         when TYPE_TIME =>
            case Right.V.Of_Type is
               when TYPE_INTEGER | TYPE_BOOLEAN | TYPE_FLOAT =>
                  return TYPE_INTEGER;

               when others =>
                  null;

            end case;

         when TYPE_FLOAT =>
            case Right.V.Of_Type is
               when TYPE_INTEGER | TYPE_BOOLEAN =>
                  return TYPE_FLOAT;

               when TYPE_TIME =>
                  return TYPE_INTEGER;

               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
      return TYPE_STRING;
   end Get_Compare_Type;

   --  ------------------------------
   --  Find the data type to be used for an arithmetic operation between two objects.
   --  ------------------------------
   function Get_Arithmetic_Type (Left, Right : Object) return Data_Type is
   begin
      if Left.V.Of_Type = TYPE_FLOAT or Right.V.Of_Type = TYPE_FLOAT then
         return TYPE_FLOAT;
      end if;
      if Left.V.Of_Type = TYPE_INTEGER or Right.V.Of_Type = TYPE_INTEGER then
         return TYPE_INTEGER;
      end if;
      if Left.V.Of_Type = TYPE_TIME or Right.V.Of_Type = TYPE_TIME then
         return TYPE_TIME;
      end if;
      if Left.V.Of_Type = TYPE_BOOLEAN and Right.V.Of_Type = TYPE_BOOLEAN then
         return TYPE_BOOLEAN;
      end if;
      return TYPE_FLOAT;
   end Get_Arithmetic_Type;

   --  ------------------------------
   --  Find the data type to be used for a composition operation between two objects.
   --  ------------------------------
   function Get_Compose_Type (Left, Right : Object) return Data_Type is
   begin
      if Left.V.Of_Type = Right.V.Of_Type then
         return Left.V.Of_Type;
      end if;
      if Left.V.Of_Type = TYPE_FLOAT or Right.V.Of_Type = TYPE_FLOAT then
         return TYPE_FLOAT;
      end if;
      if Left.V.Of_Type = TYPE_INTEGER or Right.V.Of_Type = TYPE_INTEGER then
         return TYPE_INTEGER;
      end if;
      if Left.V.Of_Type = TYPE_TIME or Right.V.Of_Type = TYPE_TIME then
         return TYPE_TIME;
      end if;
      if Left.V.Of_Type = TYPE_BOOLEAN and Right.V.Of_Type = TYPE_BOOLEAN then
         return TYPE_BOOLEAN;
      end if;
      return TYPE_FLOAT;
   end Get_Compose_Type;

   --  ------------------------------
   --  Comparison of objects
   --  ------------------------------
   generic
      with function Int_Comparator (Left, Right : Long_Long_Integer) return Boolean;
--        with function Time_Comparator (Left, Right : Ada.Calendar.Time) return Boolean;
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

         when TYPE_INTEGER =>
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

--           when TYPE_TIME =>
--              return Time_Comparator (To_Time (Left),
--                                      To_Time (Right));

         when others =>
            return False;
      end case;
   end Compare;

   function ">" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => ">",
--                                     Time_Comparator => Ada.Calendar.">",
                                   Boolean_Comparator => ">",
                                   Float_Comparator => ">",
                                   String_Comparator => ">",
                                   Wide_String_Comparator => ">");
   begin
      return Cmp (Left, Right);
   end ">";

   function "<" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "<",
--                                     Time_Comparator => Ada.Calendar."<",
                                   Boolean_Comparator => "<",
                                   Float_Comparator => "<",
                                   String_Comparator => "<",
                                   Wide_String_Comparator => "<");
   begin
      return Cmp (Left, Right);
   end "<";

   function "<=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "<=",
--                                     Time_Comparator => Ada.Calendar."<=",
                                   Boolean_Comparator => "<=",
                                   Float_Comparator => "<=",
                                   String_Comparator => "<=",
                                   Wide_String_Comparator => "<=");
   begin
      return Cmp (Left, Right);
   end "<=";

   function ">=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => ">=",
--                                     Time_Comparator => Ada.Calendar.">=",
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
--                                     Time_Comparator => Ada.Calendar."=",
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
      case Left.V.Of_Type is
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

   overriding
   procedure Adjust (Obj : in out Object) is
   begin
      case Obj.V.Of_Type is
         when TYPE_BEAN | TYPE_STRING | TYPE_WIDE_STRING =>
            if Obj.V.Proxy /= null then
               Util.Concurrent.Counters.Increment (Obj.V.Proxy.Ref_Counter);
            end if;

         when others =>
            null;

      end case;
   end Adjust;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Bean_Proxy,
                                     Name   => Bean_Proxy_Access);

   overriding
   procedure Finalize (Obj : in out Object) is
      Release : Boolean;
   begin
      case Obj.V.Of_Type is
         when TYPE_BEAN | TYPE_STRING | TYPE_WIDE_STRING =>
            if Obj.V.Proxy /= null then
               Util.Concurrent.Counters.Decrement (Obj.V.Proxy.Ref_Counter, Release);
               if Release then
                  case Obj.V.Proxy.Of_Type is
                     when TYPE_STRING =>
                        Free (Obj.V.Proxy.String_Value);

                     when TYPE_WIDE_STRING =>
                        Free (Obj.V.Proxy.Wide_String_Value);

                     when others =>
                        null;

                  end case;

                  Free (Obj.V.Proxy);
               end if;
            end if;

         when others =>
            null;

      end case;
   end Finalize;

end EL.Objects;

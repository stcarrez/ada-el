-----------------------------------------------------------------------
--  el-beans -- Bean utilities
--  Copyright (C) 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors;

with Util.Beans.Basic;

with EL.Contexts;
with EL.Expressions;
package EL.Beans is

   pragma Preelaborate;

   --  ------------------------------
   --  Bean Initialization
   --  ------------------------------
   --  The <b>Param_Value</b> describes a bean property that must be initialized by evaluating the
   --  associated EL expression.  A list of <b>Param_Value</b> can be used to initialize several
   --  bean properties of an object.  The bean object must implement the <b>Bean</b> interface
   --  with the <b>Set_Value</b> operation.
   type Param_Value (Length : Natural) is record
      Name  : String (1 .. Length);
      Value : EL.Expressions.Expression;
   end record;

   package Param_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Param_Value,
                                            "="          => "=");

   --  Add a parameter to initialize the bean property identified by the name <b>Name</b>
   --  to the value represented by <b>Value</b>.  The value string is evaluated as a string or
   --  as an EL expression.
   procedure Add_Parameter (Container : in out Param_Vectors.Vector;
                            Name      : in String;
                            Value     : in String;
                            Context   : in EL.Contexts.ELContext'Class);

   --  Initialize the bean object by evaluation the parameters and set the bean property.
   --  Each parameter expression is evaluated by using the EL context.  The value is then
   --  set on the bean object by using the bean <b>Set_Value</b> procedure.
   procedure Initialize (Bean    : in out Util.Beans.Basic.Bean'Class;
                         Params  : in Param_Vectors.Vector;
                         Context : in EL.Contexts.ELContext'Class);

end EL.Beans;

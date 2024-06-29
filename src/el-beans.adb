-----------------------------------------------------------------------
--  el-beans -- Bean utilities
--  Copyright (C) 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Objects;
package body EL.Beans is

   --  ------------------------------
   --  Add a parameter to initialize the bean property identified by the name <b>Name</b>
   --  to the value represented by <b>Value</b>.  The value string is evaluated as a string or
   --  as an EL expression.
   --  ------------------------------
   procedure Add_Parameter (Container : in out Param_Vectors.Vector;
                            Name      : in String;
                            Value     : in String;
                            Context   : in EL.Contexts.ELContext'Class) is
      Param : Param_Value (Length => Name'Length);
   begin
      Param.Name := Name;
      Param.Value := EL.Expressions.Create_Expression (Value, Context).Reduce_Expression (Context);
      Container.Append (Param);
   end Add_Parameter;

   --  ------------------------------
   --  Initialize the bean object by evaluation the parameters and set the bean property.
   --  Each parameter expression is evaluated by using the EL context.  The value is then
   --  set on the bean object by using the bean <b>Set_Value</b> procedure.
   --  ------------------------------
   procedure Initialize (Bean    : in out Util.Beans.Basic.Bean'Class;
                         Params  : in Param_Vectors.Vector;
                         Context : in EL.Contexts.ELContext'Class) is
      procedure Set_Parameter (Param : in Param_Value);

      procedure Set_Parameter (Param : in Param_Value) is
         Value : constant Util.Beans.Objects.Object := Param.Value.Get_Value (Context);
      begin
         Bean.Set_Value (Param.Name, Value);
      end Set_Parameter;

      Iter : Param_Vectors.Cursor := Params.First;
   begin
      while Param_Vectors.Has_Element (Iter) loop
         Param_Vectors.Query_Element (Iter, Set_Parameter'Access);
         Param_Vectors.Next (Iter);
      end loop;
   end Initialize;

end EL.Beans;

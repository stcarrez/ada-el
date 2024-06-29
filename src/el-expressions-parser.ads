-----------------------------------------------------------------------
--  el-expressions-parsers -- Parser for Expression Language
--  Copyright (C) 2009, 2010, 2013, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with EL.Expressions.Nodes;
private package EL.Expressions.Parser is

   pragma Preelaborate;

   procedure Parse (Expr    : in String;
                    Context : in ELContext'Class;
                    Result  : out EL.Expressions.Nodes.ELNode_Access);

   procedure Parse (Expr   : in Wide_Wide_String;
                    Context : in ELContext'Class;
                    Result : out EL.Expressions.Nodes.ELNode_Access);

end EL.Expressions.Parser;

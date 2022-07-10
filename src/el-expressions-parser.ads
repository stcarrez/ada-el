-----------------------------------------------------------------------
--  el-expressions-parsers -- Parser for Expression Language
--  Copyright (C) 2009, 2010, 2013, 2021, 2022 Stephane Carrez
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

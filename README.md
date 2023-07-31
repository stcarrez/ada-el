# Expression Language

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/elada.json)](https://alire.ada.dev/crates/elada)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-el/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-el/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-el/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-el/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-el/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-el/summary)
[![Download](https://img.shields.io/badge/download-1.8.5-brightgreen.svg)](http://download.vacs.fr/ada-el/ada-el-1.8.5.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-el)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-el/1.8.5.svg)](Commits)

This Ada05 library provides the support for a simple Expression Language
close to the Java Unified Expression Language (EL).

The API provided by the EL library is inspired from the Java
Unified Expression Language shared by the JSP 2.1 and JSF 1.2 technologies.
See Expression Language specification in JSR245
(https://jcp.org/en/jsr/summary?id=245)

The EL expression is intensively used in web development applications built
on top of various Java technologies but also on top of
[Ada Web Application](https://github.com/stcarrez/ada-awa)
and [Ada Server Faces](https://github.com/stcarrez/ada-asf).


## Version 1.8.5   - Jul 2023
  - Fix compilation with GCC 13.1
  - Fix support for EL expression with functions returning a bean object
  - Fix Get_Value to handle the Invalid_Variable exception

[List all versions](https://gitlab.com/stcarrez/ada-el/blob/master/NEWS.md)

## Build with Alire

```
alr with elada
```

## Build with configure

Build with the following commands:
```
   ./configure
   make
```

The samples can be built using:
```
   gprbuild -Psamples
```
   
The unit tests are built using:
```
   gprbuild -Ptests
```

And unit tests are executed with:
```
   bin/el_harness
```
or just
```
   make test
```

# Using Ada EL

The JSP and [JSF](http://en.wikipedia.org/wiki/JavaServer_Faces) Unified Expression Language is used to give access
to Java bean components within a presentation page (JSP, XHTML).
For JSF the expression language creates a bi-directional binding where
the value can be obtained when displaying a page but also modified (after a POST).
The unified expression language is described in [JSR-245](http://www.jcp.org/en/jsr/detail?id=245) (See also
[Unified EL](http://java.sun.com/products/jsp/reference/techart/unifiedEL.html)).

Example of presentation page:
```
  <b>${user.firstName}</b>
```

The Ada EL is a library that implements the expression language and provides
an Ada binding to use it.  The example below shows a code extract to
bind an Ada record `Joe` to the name `user` and evaluate the above expression.

```
   Ctx    : EL.Contexts.Default.Default_Context;
   E      : EL.Expressions.Expression;
   Result : EL.Objects.Object;
...
   E := EL.Expressions.Create_Expression ("${user.firstName}", Ctx);
...
   --  Bind the context to 'Joe' and evaluate
   Ctx.Set_Variable ("user", Joe);
   Result := E.Get_Value (Ctx);
```


## EL.Objects.Object

Unlike Java, Ada does not provide a root data type to represent other types (the `java.lang.Object`).  This makes the adaptation of Ada EL more difficult because
the expression language heavily relies on Java mechanisms (`Object` type and introspection).

The `EL.Objects` package provides the data type `Object` that allows to manage
entities of different types.  When an expression is evaluated, the result is
returned in an `Object`.  The record holds the value itself as well as a basic
type indicator (boolean, integer, float, string, wide wide string, ...).

The `Object` is also used to provide variable values to the expression evaluator.

To create an `Object` from a basic type, several `To_Object` functions are provided.

```
   Val : EL.Objects.Object := EL.Objects.Object.To_Object ("A string");
```

To get access to the value held by `Object`, several `To_`_type_ functions are
provided:

```
  S : constant String := EL.Objects.Object.To_String (Val);
```

The `To_`_type_ function will try to convert the value to the target type.

(See [el-objects.ads](https://github.com/stcarrez/ada-el/blob/master/src/el-objects.ads) and [util-beans-objects.ads](https://github.com/stcarrez/ada-util/blob/master/src/base/beans/util-beans-objects.ads)) 

## EL.Beans

The `EL.Beans` package defines two interfaces that allow to plug an Ada tagged
record to the expression evaluator. The `Readonly_Bean` interface defines a unique
`Get_Value` function that must be implemented.  This function is called by
the expression context resolver to find the value associated with a property.
Basically, the Ada object will be defined as a variable and associated with
a name (for example `user`).  The `Get_Value` function will be called with
the property name and the value must be returned as an `Object` (for example `firstName`).

For example:
```
   type Person is new EL.Beans.Readonly_Bean with private;

   --  Get the value identified by the name.
   function Get_Value (From : Person; Name : String) return EL.Objects.Object;
```

The `Bean` interface redefines the `Readonly_Bean` to define the `Set_Value` procedure.
This interface should be implemented when the expression evaluator has to modify
a value.

(See [util-beans-basic.ads](https://github.com/stcarrez/ada-util/blob/master/src/base/beans/util-beans-basic.ads))

## EL.Contexts

The expression language uses a context to give access to functions, variables
and resolve access to values.  The `ELContext` interface represent such context
and it gives access to:

  * A function mapper that resolves function that the evaluator can invoke.
  * A variable mapper that find the object associated with a name.
  * A resolver that will resolve properties on objects.

The function mapper is used only when parsing an expression.

The variable mapper is used to find the variable object knowing its name.
For example it will resolve the name `user` and return an instance of the
`Readonly_Bean` interface (a `Person`).

The resolver will resolve the variable to obtain the value from the property name.

The `EL.Contexts` package defines the `ELResolver` and `ELContext` interfaces.
The `EL.Contexts.Default` package provides default implementation of these interfaces.

(See [el-contexts.ads](https://github.com/stcarrez/ada-el/blob/master/src/el-contexts.ads))

## EL.Functions

The `EL.Functions` package defines the `Function_Mapper` interface that allows to
register functions for the expression parser.  The evaluator will invoke the
functions directly (without the need of the `Function_Mapper`).

A function can get from one to four arguments (this is pre-defined because Ada does
not support variable argument lists easily).  Each argument is recieved as
an `Object`.  The function must returns an `Object` value.  

The function below returns the year part of a date.  The date is retrieved as
an `Ada.Calendar.Time` and the result will be returned as an integer.
```
function Year (Val : EL.Objects.Object) return EL.Objects.Object is
   Date : constant Ada.Calendar.Time := To_Time (Val);
begin
   return To_Object (Ada.Calendar.Formatting.Year (Date));
end Format;
```

The function will be registered as follows:

```
   Fm  : constant EL.Functions.Function_Mapper_Access
     := new EL.Functions.Default.Default_Function_Mapper;
...
   Fm.Set_Function ("year", Year'Access);
```

(See [el-functions.ads](https://github.com/stcarrez/ada-el/blob/master/src/el-functions.ads))

## EL.Variables

The `EL.Variables` package defines the `VariableMapper` interface and
the `EL.Variables.Default` package provides a default implementation.
The `VariableMapper` allows to bind a name to an Ada object that implements
the `EL.Beans.Readonly_Bean` or `EL.Beans.Bean` interfaces (in Java, one would
be able to use any Java object).

```
   Joe  : constant Person_Access := Create_Person ("Joe", "Smith", 12);

   Ctx.Set_Variable ("user", Joe);
```

(See [el-variables.ads](https://github.com/stcarrez/ada-el/blob/master/src/el-variables.ads))

## EL.Expressions

The `EL.Expressions` package is the main package to parse and evaluate expressions.
An expression string is parsed using the `Create_Expression` function which
returns an `Expression` record.  The expression is parsed only once and it
can be evaluated several times.  The expression context is used only to get
access to the function mapper.

```
   E : EL.Expressions.Expression := Create_Expression ("${user.firstName}", Ctx);
```

The expression is evaluated using the `Get_Value` function.  The evaluation is made
on the expression context which gives access to the variables and the resolver.
The expression context should be a per-thread object.  The expression can be
shared by several threads and evaluated at the same time.

```
  Val : EL.Objects.Object := E.Get_Value (Ctx);
```

(See [el-expressions.ads](https://github.com/stcarrez/ada-el/blob/master/src/el-expressions.ads))

# Class Diagram

![Class diagram](doc/Expression.png)



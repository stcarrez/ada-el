# Expression Language

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada%20EL.svg)](http://jenkins.vacs.fr/job/Ada%20EL/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada%20EL.svg)](http://jenkins.vacs.fr/job/Ada%20EL/)
[![Download](https://img.shields.io/badge/download-1.5.1-brightgreen.svg)](http://download.vacs.fr/ada-el/ada-el-1.5.1.tar.gz)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-el/ada-el-1.5.1.svg)

This Ada05 library provides the support for a simple Expression Language
close to the Java Unified Expression Language (EL).

The API provided by the EL library is inspired from the Java
Unified Expression Language shared by the JSP 2.1 and JSF 1.2 technologies.
See Expression Language specification in JSR245
(http://jcp.org/en/jsr/summary?id=245)

Build with the following commands:
```
   ./configure
   make
```

The samples can be built using:
```
   gnatmake -Psamples
```
   
The unit tests are built using:
```
   gnatmake -Ptests
```

And unit tests are executed with:
```
   bin/el_harness
```
or just
```
   make test
```
# Documentation

The Ada EL sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-el/wiki



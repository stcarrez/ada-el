# Expression Language

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/elada.json)](https://alire.ada.dev/crates/elada)
[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Ada-EL.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-EL/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Ada-EL.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-EL/)
[![codecov](https://codecov.io/gh/stcarrez/ada-el/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-el)
[![Download](https://img.shields.io/badge/download-1.8.5-brightgreen.svg)](http://download.vacs.fr/ada-el/ada-el-1.8.5.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-el)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-el/1.8.5.svg)

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


# Version 1.8.5   - Aug 2022
  - Cleanup the examples

[List all versions](https://github.com/stcarrez/ada-el/blob/master/NEWS.md)

# Build

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



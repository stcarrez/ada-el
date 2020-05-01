# Expression Language

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/elada.json)](https://alire.ada.dev/crates/elada)
[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Ada-EL.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-EL/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Ada-EL.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-EL/)
[![codecov](https://codecov.io/gh/stcarrez/ada-el/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-el)
[![Download](https://img.shields.io/badge/download-1.7.0-brightgreen.svg)](http://download.vacs.fr/ada-el/ada-el-1.7.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-el/1.7.0.svg)

This Ada05 library provides the support for a simple Expression Language
close to the Java Unified Expression Language (EL).

The API provided by the EL library is inspired from the Java
Unified Expression Language shared by the JSP 2.1 and JSF 1.2 technologies.
See Expression Language specification in JSR245
(https://jcp.org/en/jsr/summary?id=245)

# Version 1.8.0 - May 2020

* New Expand procedure to expand the properties in place

(Other versions)[NEWS]

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



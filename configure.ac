dnl Autoconf configure script for Ada EL
dnl Copyright (C) 2001 - 2019 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/el.ads)

# Current release settings
EL_MAJOR_VERSION=1
EL_MINOR_VERSION=6
EL_MICRO_VERSION=1
EL_VERSION=$EL_MAJOR_VERSION.$EL_MINOR_VERSION.$EL_MICRO_VERSION

# Set the version number of Ada EL
AC_SUBST(EL_VERSION)
AC_SUBST(EL_MAJOR_VERSION)
AC_SUBST(EL_MINOR_VERSION)
AC_SUBST(EL_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(elada)

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_base],
  [git@github.com:stcarrez/ada-util.git],
  [Building Ada EL requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_utilada_base}
  ])
AC_SUBST(UTIL_DIR)

AC_OUTPUT(
Makefile.conf elada.gpr elada_tests.gpr
)

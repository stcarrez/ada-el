dnl Autoconf configure script for Ada EL
dnl Copyright (C) 2001 - 2023 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl SPDX-License-Identifier: Apache-2.0
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT
AC_CONFIG_SRCDIR([src/el.ads])

# Current release settings
EL_MAJOR_VERSION=1
EL_MINOR_VERSION=8
EL_MICRO_VERSION=6
EL_VERSION=$EL_MAJOR_VERSION.$EL_MINOR_VERSION.$EL_MICRO_VERSION

# Set the version number of Ada EL
AC_SUBST(EL_VERSION)
AC_SUBST(EL_MAJOR_VERSION)
AC_SUBST(EL_MINOR_VERSION)
AC_SUBST(EL_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(elada)
AM_GNAT_FIND_ADA_UTIL([Ada EL])

AC_CONFIG_FILES([Makefile.conf elada.gpr elada_tests.gpr
])
AC_OUTPUT

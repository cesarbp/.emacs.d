AC_INIT(emacs-test-unit, 0.01,)
AC_CONFIG_SRCDIR(test-unit.el)
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

##
## Find out where to install the debugger emacs lisp files
##
AM_PATH_LISPDIR
AM_CONDITIONAL(INSTALL_EMACS_LISP, test "x$lispdir" != "x")

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

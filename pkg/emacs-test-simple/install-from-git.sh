#!/bin/bash
# This installs all emcs-test-simple and its prerequisites. If you are lucky
# you can just run this:
#
#   bash ./install-from-git.sh
#
# However we do provide for some customization...
#
# 1. GIT PROTOCOL
# ===============
#
# If your "git clone" can't handle the "http" protocol, you might be
# able to use the "git" protocol. To do this set the GIT_PROTOCOL
# variable like this:
#
#     GIT_PROTOCOL=git sh ./install-from-git.sh
#
# 2. configure options (e.g --prefix)
# ====================================

# If you want to customize configuration parameters, for example,
# choose where to install, you can pass configure options to this
# script. For example:# can pass configure options.
#
#     sh ./install-from-git.sh --prefix=/tmp
#
# 3. TO "sudo" or not to "sudo"?
# ==============================
# If you are running as root on a *Nix-like box, then there's no problem.
#
# If you are not running as root, "sudo" might be invoked to install
# code.  On systems that don't have a "sudo" command but need
# filesystem permission, then you get by with setting SUDO_CMD to "su root-c"
# For example:
#
#    SUDO_CMD='su root -c' sh ./install-from-git.sh
#
# If you have sufficient filesystem permission (which is often the
# case on Windows or cygwin) then you might not need or want sudo. So
# here, set SUDO_CMD to a blank:
#
#      SUDO_CMD=' ' sh ./install-from-git.sh
#
#
# To finish here is an invocation using all 3 above options:
#   GIT_PROTOCOL='git' SUDO_CMD=' ' sh ./install-from-git.sh --prefix=/tmp

GIT_PROTOCOL=${GIT_PROTOCOL:-http}

run_cmd() {
    echo "--- Running command: $@"
    $@
    rc=$?
    echo "--- $@ exit status is $?"
    return $rc
}

if (( $(id -u) != 0)) ; then
    if [[ -z "$SUDO_CMD" ]] ; then
	need_sudo='sudo'
	if which $need_sudo >/dev/null 2>&1 ; then
	    try_cmd=''
	else
	    need_sudo='su root -c'
	    try_cmd='su'
	fi
    else
	need_sudo="$SUDO_CMD"
    fi
else
    need_sudo=''
    try_cmd=''
fi

for program in git make $try_cmd ; do
    if ! which $program >/dev/null 2>&1 ; then
	echo "Cant find program $program in $PATH"
	exit 1
    fi
done

for pkg in emacs-test-simple ; do
    echo '******************************************'
    echo Trying to install ${pkg}...
    echo '******************************************'
    run_cmd git clone ${GIT_PROTOCOL}://github.com/rocky/${pkg}.git
    (cd $pkg && \
        run_cmd $SHELL ./autogen.sh && \
	run_cmd ./configure $@ && \
	run_cmd make && \
	run_cmd make check && \
        run_cmd $need_sudo make install
    )
done

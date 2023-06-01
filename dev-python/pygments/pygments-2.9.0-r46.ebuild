# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#   Time-stamp: <>
#   Touched: Thu Jun 01 11:39:19 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230601 reinstate py_3[78] on slot 46

EAPI=8

PYPI_NO_NORMALIZE=1
PYPI_PN=${PN^}
PYTHON_COMPAT=( python3_{7..8} )

inherit distutils-r1 bash-completion-r1 pypi

MY_P=${P^}
DESCRIPTION="Pygments is a syntax highlighting package written in Python"
HOMEPAGE="
	https://pygments.org/
	https://github.com/pygments/pygments/
	https://pypi.org/project/Pygments/"
SRC_URI="mirror://pypi/${MY_P:0:1}/${PN^}/${MY_P}.tar.gz"
S=${WORKDIR}/${MY_P}

LICENSE="BSD"
SLOT="46"
KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~x64-cygwin ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~sparc-solaris ~sparc64-solaris ~x64-solaris ~x86-solaris"

BDEPEND="
	test? (
		virtual/ttf-fonts
	)"

if [ ! "$SLOT" = 46 ]; then
distutils_enable_sphinx doc
fi

distutils_enable_tests pytest

python_install_all() {
	distutils-r1_python_install_all
	if [ ! "$SLOT" = "46" ]; then
	newbashcomp external/pygments.bashcomp pygmentize
	else
		mv ${ED}/usr/bin/pygmentize{,_46}
	fi
}

# Copyright 2004-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

#
#   Time-stamp: <>
#   Touched: Fri Oct 21 14:44:44 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 221021 2.4.7-r1 - support setuptools-46 see comment in dev-python/more-itertools-8.3.0.ebuild

EAPI=7
_PYTHON_ALLOW_PY37=1
PYTHON_COMPAT_NO_STRICT=true
PYTHON_COMPAT=( python3_{7..10} pypy3 )

inherit distutils-r1

MY_P=${P/-/_}
DESCRIPTION="Easy-to-use Python module for text parsing"
HOMEPAGE="https://github.com/pyparsing/pyparsing https://pypi.org/project/pyparsing/"
SRC_URI="https://github.com/${PN}/${PN}/archive/${MY_P}.tar.gz"
S=${WORKDIR}/${PN}-${MY_P}

LICENSE="MIT"
SLOT="46"
KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~x64-cygwin ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~sparc-solaris ~sparc64-solaris ~x64-solaris ~x86-solaris"
IUSE="examples"

distutils_enable_tests setup.py

BDEPEND="
	=dev-python/setuptools-46.3.0-r2:46[${PYTHON_USEDEP}]
"

python_install_all() {
	if use examples; then
		docompress -x /usr/share/doc/${PF}/examples
		dodoc -r examples
	fi
	distutils-r1_python_install_all
}

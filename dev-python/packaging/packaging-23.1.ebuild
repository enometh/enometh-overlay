# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed May 31 23:42:34 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230531 23.1 drop setuptools rdep, portage fails on conflicts with slot 46

# please keep this ebuild at EAPI 8 -- sys-apps/portage dep
EAPI=8

DISTUTILS_USE_PEP517=flit
PYTHON_COMPAT=( python3_{10..12} pypy3 )

inherit distutils-r1 pypi

DESCRIPTION="Core utilities for Python packages"
HOMEPAGE="
	https://github.com/pypa/packaging/
	https://pypi.org/project/packaging/
"

SLOT="0"
LICENSE="|| ( Apache-2.0 BSD-2 )"
KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"

# ;madhu 230531 RDEP 	!<dev-python/setuptools-67
RDEPEND="
	>=dev-python/pyparsing-3.0.7-r1[${PYTHON_USEDEP}]
"
DEPEND="
	test? (
		dev-python/pretend[${PYTHON_USEDEP}]
	)
"

distutils_enable_tests pytest

python_test() {
	epytest --capture=no
}

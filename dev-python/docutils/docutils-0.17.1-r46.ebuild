# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Jun 01 11:41:01 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 210630 0.17.1
# ;madhu 230601 0.17.1-r46 reinstate (py3[78]) drop docs

EAPI=8

PYTHON_COMPAT=( python3_{7..8} )
inherit distutils-r1 pypi
RESTRICT="test"

DESCRIPTION="Python Documentation Utilities (reference reStructuredText impl.)"
HOMEPAGE="
	https://docutils.sourceforge.io/
	https://pypi.org/project/docutils/
"

LICENSE="BSD-2 GPL-3 public-domain"
SLOT="46"
KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~x64-solaris"

RDEPEND="
	dev-python/pygments:46[${PYTHON_USEDEP}]
"
BDEPEND="
	${RDEPEND}
"

PATCHES=(
	"${FILESDIR}/docutils-0.18.1-py311.patch"
)

src_test() {
	cd test || die
	distutils-r1_src_test
}

python_install_all() {
	distutils-r1_python_install_all
	rm -fv ${ED}/usr/bin/*
}

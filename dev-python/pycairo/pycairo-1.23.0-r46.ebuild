# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Jun 30 16:47:26 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210630 pycairo-1.20.1 (re-add python_37) (upstream dropped python_27)
# this is slot 0. pycairo-1.18.2-r1 is slot 2
# ;madhu 230601 1.23.0 SLOT 46 fpr python3_[78]

EAPI="8"

DISTUTILS_EXT=1
PYTHON_COMPAT=( python3_{7..8} pypy3)
PYTHON_REQ_USE="threads(+)"

inherit distutils-r1

DESCRIPTION="Python bindings for the cairo library"
HOMEPAGE="https://www.cairographics.org/pycairo/ https://github.com/pygobject/pycairo
	https://pypi.org/project/pycairo/"
SRC_URI="https://github.com/pygobject/${PN}/releases/download/v${PV}/${P}.tar.gz"

LICENSE="|| ( LGPL-2.1 MPL-1.1 )"
SLOT="46"
KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~loong ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos"
IUSE="examples"
RESTRICT="test"

BDEPEND="
	virtual/pkgconfig
	test? (
		dev-python/hypothesis[${PYTHON_USEDEP}]
	)
"
RDEPEND="
	>=x11-libs/cairo-1.15.10[svg(+)]
"
DEPEND="
	${RDEPEND}
	x11-base/xorg-proto
"

PATCHES=(
	"${FILESDIR}"/${PN}-1.23.0-fix-sphinx.patch
)

distutils_enable_sphinx docs \
	dev-python/sphinx-rtd-theme
distutils_enable_tests pytest

python_test() {
	esetup.py build_tests
	epytest
}

python_install() {
	distutils-r1_python_install
}

python_install_all() {

	if [ $SLOT != "46" ]; then
	if use examples; then
		dodoc -r examples
	fi
	fi

	distutils-r1_python_install_all
	if [ $SLOT = "46" ]; then
		rm -fv ${ED}/usr/include/pycairo/py3cairo.h
	fi
}

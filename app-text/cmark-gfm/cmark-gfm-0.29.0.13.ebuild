# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Mar 09 06:29:52 2025 -0600 <madhu@cs.unm.edu>
#   Bugs-To: madhu@cs.unm.edu
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.

# ;madhu 250309 - 0.29.0.13

EAPI=8

MYPV="$(ver_rs 3 '.gfm.')"
PYTHON_COMPAT=( python3_{10..13} )

inherit cmake python-any-r1

DESCRIPTION="GitHub's fork of cmark"
HOMEPAGE="https://github.com/github/cmark-gfm"
SRC_URI="https://github.com/github/cmark-gfm/archive/${MYPV}.tar.gz -> ${P}.tar.gz"
S="${WORKDIR}/${PN}-${MYPV}"

LICENSE="BSD-2"
SLOT="0/0.29.0"
KEYWORDS="~amd64"
IUSE="test"

DEPEND="test? ( ${PYTHON_DEPS} )"

RESTRICT="!test? ( test )"

pkg_setup() {
	use test && python-any-r1_pkg_setup
}

src_configure() {
	local mycmakeargs=(
		-DCMARK_LIB_FUZZER=OFF
		-DCMARK_SHARED=ON
		-DCMARK_STATIC=OFF
		-DCMARK_TESTS="$(usex test)"
	)
	cmake_src_configure
}

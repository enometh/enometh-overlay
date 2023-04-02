# Copyright 1997-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Mar 24 11:33:23 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230324 pocketsphinx-5.0.0 (WIP, STUB)
#
EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{9..11} )
inherit cmake distutils-r1

DESCRIPTION="One of Carnegie Mellon University's open source large vocabulary, speaker-independent continuous speech recognition engines"
HOMEPAGE="https://github.com/cmusphinx/pocketsphinx"
SRC_URI="https://github.com/cmusphinx/pocketsphinx/archive/refs/tags/v${PN}.tar.gz -> ${P}.tar.gz"
IUSE="gstreamer fixed-point sphinx-debug"

LICENSE="BSD"					#notrly FIXME: LICENSE is complex.
SLOT="0"
KEYWORDS="~x86 ~amd64"

RDEPEND="
	gstreamer? ( media-libs/gstreamer:1.0 )
"
DEPEND="
	${RDEPEND}
"

BDEPEND="
	>=dev-python/cython-0.27
	>=dev-python/scikit-build-0.15[${PYTHON_USEDEP}]
"

src_prepare() {
	cmake_src_prepare
	distutils-r1_src_prepare
}

src_configure() {
	local mycmakeargs=(
		-DBUILD_GSTREAMER=$(usex gstreamer)
		-DFIXED_POINT=$(usex fixed-point)
	)
	if use sphinx-debug; then
		mycmakeargs+=( -DSPHINX_DEBUG=1 )
	fi
	cmake_src_configure
	distutils-r1_src_configure
}

src_compile() {
	cmake_src_compile
	distutils-r1_src_compile
}

src_install() {
	cmake_src_install
	distutils-r1_src_install
}

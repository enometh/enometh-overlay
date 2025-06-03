# Copyright 1997-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Mar 24 11:33:23 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230324 pocketsphinx-5.0.0 (WIP, STUB)
# ;madhu 250603 5.0.4 USE_GIT=true

EAPI=8

USE_GIT=true

DISTUTILS_USE_PEP517=scikit-build-core
PYTHON_COMPAT=( python3_{11..13} )
inherit cmake distutils-r1

DESCRIPTION="One of Carnegie Mellon University's open source large vocabulary, speaker-independent continuous speech recognition engines"
HOMEPAGE="https://github.com/cmusphinx/pocketsphinx"
if ${USE_GIT}; then
   inherit git-r3
   EGIT_REPO_URI="https://github.com/cmusphinx/pocketsphinx"
   EGIT_BRANCH=master
else
	SRC_URI="https://github.com/cmusphinx/pocketsphinx/archive/refs/tags/v${PN}.tar.gz -> ${P}.tar.gz"
fi

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
	>=dev-python/scikit-build-core-0.11[${PYTHON_USEDEP}]
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

# Copyright 2023-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 23:01:37 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 1.3.4  dont run any of the 247MB of tests/

EAPI=8

inherit cmake

DESCRIPTION="read AES SOFA files with HRTFs stored in th  AES69-2015 standard"
HOMEPAGE="https://github.com/hoene/libmysofa"

SRC_URI="https://github.com/hoene/libmysofa/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"
KEYWORDS="~amd64"

LICENSE="BSD-with-attribution"
SLOT="0"

src_configure() {
	local mycmakeargs=(
		-DBUILD_TESTS="off"
		-DBUILD_STATIC_LIBS="off"
	)

	cmake_src_configure
}

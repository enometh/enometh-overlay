# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Mar 24 17:20:31 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230324 3.6.11 (WIP)

EAPI=8
inherit cmake-multilib

HOMEPAGE="https://github.com/pedrolcl/sonivox"
DESCRIPTION="Fork of the AOSP 'platform_external_sonivox' project to use it outside of Android"
SRC_URI="https://github.com/pedrolcl/sonivox/archive/refs/tags/v${PN}.tar.gz -> ${P}.tar.gz"

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="~x86 ~amd64"
IUSE="static-libs examples"

multilib_src_configure() {
	local mycmakeargs=(
		-DBUILD_SONIVOX_STATIC=$(usex static-libs)
		-DBUILD_TESTING=OFF
	)
	cmake_src_configure
}

multilib_src_install_all() {
	if use examples; then
		DOCS+=(example)
		docompress -x /usr/share/doc/${PF}/example
	fi
	einstalldocs
}
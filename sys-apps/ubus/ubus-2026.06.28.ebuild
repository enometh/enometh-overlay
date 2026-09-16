# Copyright 2024-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Sep 16 09:35:07 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260916 2026.07.21:  USE_GIT only. TODO: LUADEPS

EAPI=8

inherit cmake

USE_GIT="true"

DESCRIPTION="OpenWrt utility and data structures library"
HOMEPAGE="https://openwrt.org/docs/techref/ubus"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://git.openwrt.org/project/ubus.git"
	EGIT_CLONE_TYPE=shallow
	EGIT_BRACH=main
	EGIT_COMMIT=24864e7840b3a02a9ef76284a373f6b2f00b8a9b
	EGIT_OVERRIDE_REPO_PROJECT_UBUS="file:///build/git-mirror/ubus.git"
else
	eerror "not implemented yet"
fi

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="~amd64"

RDEPEND="
	dev-libs/libubox
"
DEPEND="${RDEPEND}"

PATCHES=(
	$FILESDIR/ubus-2026.06.27-cmake-adjust-destinations-for-multilib.patch
)
src_configure() {
	local mycmakeargs=(
		#-DBUILD_LUA=OFF
		-DCMAKE_INSTALL_LIBDIR:PATH="$(get_libdir)"
	)
	cmake_src_configure
}

src_install() {
	cmake_src_install
}

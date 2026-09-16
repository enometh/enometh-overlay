# Copyright 2024-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Sep 16 09:35:07 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260916 2026.07.21:  USE_GIT only.

EAPI=8

inherit cmake-multilib

USE_GIT="true"

DESCRIPTION="OpenWrt utility and data structures library"
HOMEPAGE="https://openwrt.org/docs/techref/libubox"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://git.openwrt.org/project/libubox.git"
	EGIT_CLONE_TYPE=shallow
	EGIT_BRACH=main
	EGIT_COMMIT=e7608b69283d919d031d13cc8e21692503f5dbea
	EGIT_OVERRIDE_REPO_PROJECT_LIBUBOX="file:///build/git-mirror/libubox.git"
else
	eerror "not implemented yet"
fi

LICENSE="ISC"
SLOT="0"
KEYWORDS="~amd64"

RDEPEND="
	dev-libs/json-c[${MULTILIB_USEDEP}]
"
DEPEND="${RDEPEND}"

PATCHES=(
	${FILESDIR}/libubox-2026.07.21-cmake-adjust-destinations-for-multilib.patch
)

multilib_src_configure() {
	local mycmakeargs=(
		# ;madhu 260916 TODO IUSE=lua once you that cen be isolated from
		# gentoo
		-DBUILD_LUA=OFF
		-DCMAKE_INSTALL_LIBDIR:PATH="$(get_libdir)"
	)
	cmake_src_configure
}

multilib_src_install() {
	cmake_src_install
}

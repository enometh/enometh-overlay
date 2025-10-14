# Copyright 2023-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Feb 05 23:56:13 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 241115 - 0.4.5
# ;madhu 250205 - 0.5.1
# ;madhu 250205 - 0.7.2
# ;madhu 250408 - 0.8.0
# ;madhu 250729 - 0.9.2
# ;madhu 251014 - 0.0.5

EAPI=8

inherit cmake

DESCRIPTION="Aquamarine is a very light linux rendering backend library"
HOMEPAGE="https://github.com/hyprwm/aquamarine"

if [[ "${PV}" = *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/hyprwm/${PN^}.git"
else
	SRC_URI="https://github.com/hyprwm/${PN^}/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="amd64"
fi

LICENSE="BSD"
SLOT="0"
# SLOT="0/$(ver_cut 1-2)"

# Upstream states that the simpleWindow test is broken, see bug 936653
RESTRICT="test"
# 	media-libs/mesa
RDEPEND="
	>=dev-libs/libinput-1.26.1
	dev-libs/wayland
	media-libs/libdisplay-info
	media-libs/mesa
	>=dev-util/hyprwayland-scanner-0.4.0
	>=gui-libs/hyprutils-0.8.0
	sys-apps/hwdata
	>=sys-auth/seatd-0.8.0
	x11-libs/cairo
	x11-libs/libxkbcommon
	x11-libs/libdrm
	x11-libs/pango
	x11-libs/pixman
	virtual/libudev
"
DEPEND="
	${RDEPEND}
	dev-libs/wayland-protocols
"

BDEPEND="
	dev-util/wayland-scanner
	virtual/pkgconfig
"

src_prepare() {
	sed -i "/add_compile_options(-O3)/d" "${S}/CMakeLists.txt" || die
	cmake_src_prepare
}

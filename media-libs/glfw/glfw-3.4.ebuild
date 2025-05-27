# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue May 27 20:26:32 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250527 - 3.4. 3.4-23-ge7ea71be (USE_GIT=true) IUSE=examples, doc

EAPI=8
USE_GIT=true

inherit cmake-multilib

DESCRIPTION="Portable OpenGL FrameWork"
HOMEPAGE="https://www.glfw.org/"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/glfw/glfw.git"
#   EGIT_OVERRIDE_REPO_GLFW_GLFW=file:///build/git-mirror/glfw.git
#	EGIT_BRANCH=master
	EGIT_CLONE_TYPE=shallow
else
SRC_URI="https://github.com/glfw/glfw/archive/${PV}.tar.gz -> ${P}.tar.gz"
fi

LICENSE="ZLIB"
SLOT="0"
KEYWORDS="amd64 ~arm ~arm64 ~hppa ~ppc64 ~riscv x86"
IUSE="wayland X examples doc"

# Most are dlopen'd so use strings or check the source:
# grep -Eiro '[a-z0-9-]+\.so\.[0-9]+'
DEPEND="
	wayland? (
		dev-libs/wayland[${MULTILIB_USEDEP}]
		dev-libs/wayland-protocols
	)
	X? (
		x11-base/xorg-proto
		x11-libs/libX11[${MULTILIB_USEDEP}]
		x11-libs/libXcursor[${MULTILIB_USEDEP}]
		x11-libs/libXi[${MULTILIB_USEDEP}]
		x11-libs/libXinerama[${MULTILIB_USEDEP}]
		x11-libs/libxkbcommon[${MULTILIB_USEDEP}]
		x11-libs/libXrandr[${MULTILIB_USEDEP}]
	)
"

# 	media-libs/libglvnd[X?,${MULTILIB_USEDEP}]

#	wayland? (
# 		gui-libs/libdecor[${MULTILIB_USEDEP}]
#	)

RDEPEND="
	${DEPEND}
	X? (
		x11-libs/libXrender[${MULTILIB_USEDEP}]
		x11-libs/libXxf86vm[${MULTILIB_USEDEP}]
	)
	doc? ( app-text/doxygen )
"
BDEPEND="
	wayland? (
		dev-util/wayland-scanner
		kde-frameworks/extra-cmake-modules
	)
"

src_configure() {
	local mycmakeargs=(
		-DBUILD_SHARED_LIBS=on
		-DGLFW_BUILD_EXAMPLES=$(usex examples)
		-DGLFW_BUILD_DOCS=$(usex doc)
		-DGLFW_BUILD_WAYLAND=$(usex wayland)
		-DGLFW_BUILD_X11=$(usex X)
	)

	cmake-multilib_src_configure
}

multilib_src_install() {
	cmake_src_install
	if multilib_is_native_abi; then
		exeinto usr/$(get_libdir)/glfw3
		doexe examples/{boing,gears,heightmap,offscreen,particles,sharing,splitview,triangle-opengl,triangle-opengles,wave,windows}
	fi
}

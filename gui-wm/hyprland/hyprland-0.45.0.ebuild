# Copyright 2023-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Nov 17 06:08:31 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241117 0.45.0 - patch for gcc-13, by replacing <print> with libfmt, need to unmask libfmt. bundle hyprland-protocols, bump down cmake, generate version.h,
# set ALTSRC=false, set SRC_URI and update manifest to build with gh sources
# ln -sv /16/tmp.d/spoilers.d/Hyprland-0.45.0.7z /gentoo/distfiles

EAPI=8
ALTSRC=false

inherit meson toolchain-funcs

DESCRIPTION="A dynamic tiling Wayland compositor that doesn't sacrifice on its looks"
HOMEPAGE="https://github.com/hyprwm/Hyprland"

if [[ "${PV}" = *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/hyprwm/${PN^}.git"
else

if ${ALTSRC}; then
	SRC_URI="https://github.com/hyprwm/${PN^}/releases/download/v${PV}/source-v${PV}.tar.gz -> Hyprland-${PV}.7z"
	S=${WORKDIR}/Hyprland-${PV}
	inherit unpacker
else
	SRC_URI="https://github.com/hyprwm/${PN^}/releases/download/v${PV}/source-v${PV}.tar.gz -> ${P}.gh.tar.gz"
	S="${WORKDIR}/${PN}-source"
fi

	KEYWORDS="~amd64"
fi

LICENSE="BSD"
SLOT="0"
IUSE="X legacy-renderer systemd"

# hyprpm (hyprland plugin manager) requires the dependencies at runtime
# so that it can clone, compile and install plugins.
HYPRPM_RDEPEND="
	app-alternatives/ninja
	>=dev-build/cmake-3.29.3
	dev-build/meson
	dev-vcs/git
	virtual/pkgconfig
"
RDEPEND="
	${HYPRPM_RDEPEND}
	dev-cpp/tomlplusplus
	dev-libs/glib:2
	>=dev-libs/libfmt-11.0.2
	dev-libs/libinput
	>=dev-libs/udis86-1.7.2
	>=dev-libs/wayland-1.22.90
	>=gui-libs/aquamarine-0.4.3
	>=gui-libs/hyprcursor-0.1.9
	media-libs/libglvnd
	x11-libs/cairo
	x11-libs/libdrm
	x11-libs/libxkbcommon
	x11-libs/pango
	x11-libs/pixman
	x11-libs/libXcursor
	X? (
		x11-libs/libxcb:0=
		x11-base/xwayland
		x11-libs/xcb-util-errors
		x11-libs/xcb-util-wm
	)
"
# 	>=dev-libs/hyprlang-0.3.2
# 	>=dev-libs/hyprland-protocols-0.4
DEPEND="
	${RDEPEND}
	>=dev-libs/wayland-protocols-1.36
	>=gui-libs/hyprutils-0.2.5
	>=gui-libs/hyprcursor-0.1.9
"
BDEPEND="
	|| ( >=sys-devel/gcc-13:* >=sys-devel/clang-18:* )
	app-misc/jq
	dev-build/cmake
	>=dev-util/hyprwayland-scanner-0.3.10
	virtual/pkgconfig
"

pkg_setup() {
	[[ ${MERGE_TYPE} == binary ]] && return

#	if tc-is-gcc && ver_test $(gcc-version) -lt 14 ; then
#		eerror "Hyprland requires >=sys-devel/gcc-14 to build"
#		eerror "Please upgrade GCC: emerge -v1 sys-devel/gcc"
#		die "GCC version is too old to compile Hyprland!"
#   elif
	if tc-is-clang && ver_test $(clang-version) -lt 18 ; then
		eerror "Hyprland requires >=sys-devel/clang-18 to build"
		eerror "Please upgrade Clang: emerge -v1 sys-devel/clang"
		die "Clang version is too old to compile Hyprland!"
	fi
}

PATCHES=(
$FILESDIR/hyprland-0.45.0-meson.build-revert-to-c-23.patch
$FILESDIR//hyprland-0.45.0-replace-c-26-print-with-fmt-11.0.2.patch
)

src_prepare() {
	# skip version.h
#	sed -i -e "s|scripts/generateVersion.sh|echo|g" meson.build || die
	default
}

src_configure() {
	local emesonargs=(
		$(meson_feature legacy-renderer legacy_renderer)
		$(meson_feature systemd)
		$(meson_feature X xwayland)
	)

	meson_src_configure
}

# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sat Dec 10 20:45:46 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 190118 1.16.0
# ;madhu 221210 1.17.6 - keep svg gles2 + opengl, also utils (unreleased)
# ;madhu 230507 1.17.8
# ;madhu 240623 1.18.0 -- fix gtk-doc
# ;madhu 250710 1.18.4-r1

EAPI=8

PYTHON_COMPAT=( python3_{11..13} )
inherit meson-multilib python-any-r1

if [[ ${PV} == *9999* ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://gitlab.freedesktop.org/cairo/cairo.git"
else
	SRC_URI="https://gitlab.freedesktop.org/cairo/cairo/-/archive/${PV}/cairo-${PV}.tar.bz2"
	KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 ~sparc x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"
fi

DESCRIPTION="A vector graphics library with cross-device output support"
HOMEPAGE="https://www.cairographics.org/ https://gitlab.freedesktop.org/cairo/cairo"
LICENSE="|| ( LGPL-2.1 MPL-1.1 )"
SLOT="0"
IUSE="X aqua debug +glib gtk-doc lzo test"
IUSE+=" utils gles2-only gles3 opengl svg gles2"
# Tests need more wiring up like e.g. https://gitlab.freedesktop.org/cairo/cairo/-/blob/master/.gitlab-ci.yml
# any2ppm tests seem to hang for now.
RESTRICT="test !test? ( test )"

RDEPEND="
	>=media-libs/fontconfig-2.13.92[${MULTILIB_USEDEP}]
	>=media-libs/freetype-2.13:2[png,${MULTILIB_USEDEP}]
	>=media-libs/libpng-1.6.10:0=[${MULTILIB_USEDEP}]
	>=sys-libs/zlib-1.2.8-r1[${MULTILIB_USEDEP}]
	>=x11-libs/pixman-0.42.3[${MULTILIB_USEDEP}]
	debug? ( sys-libs/binutils-libs:0=[${MULTILIB_USEDEP}] )
	glib? ( >=dev-libs/glib-2.34.3:2[${MULTILIB_USEDEP}] )
	lzo? ( >=dev-libs/lzo-2.06-r1:2[${MULTILIB_USEDEP}] )
	X? (
		>=x11-libs/libXrender-0.9.8[${MULTILIB_USEDEP}]
		>=x11-libs/libXext-1.3.2[${MULTILIB_USEDEP}]
		>=x11-libs/libX11-1.6.2[${MULTILIB_USEDEP}]
		>=x11-libs/libxcb-1.9.1:=[${MULTILIB_USEDEP}]
	)"
DEPEND="${RDEPEND}
	test? (
		app-text/ghostscript-gpl
		app-text/poppler[cairo]
		gnome-base/librsvg
	)
	X? ( x11-base/xorg-proto )"
BDEPEND="
	${PYTHON_DEPS}
	virtual/pkgconfig
	gtk-doc? ( dev-util/gtk-doc )"

PATCHES=(
	"${FILESDIR}"/${PN}-respect-fontconfig.patch
)

multilib_src_configure() {
	local emesonargs=(
		-Ddwrite=disabled
		-Dfontconfig=enabled
		-Dfreetype=enabled
		-Dpng=enabled
		$(meson_feature aqua quartz)
		$(meson_feature X tee)
		$(meson_feature X xcb)
		$(meson_feature X xlib)
		-Dxlib-xcb=disabled
		-Dzlib=enabled

		# Requires poppler-glib (poppler[cairo]) which isn't available in multilib
		$(meson_native_use_feature test tests)

		$(meson_feature lzo)
		$(meson_feature utils gtk2-utils)

		$(meson_feature glib)
		-Dspectre=disabled # only used for tests
		$(meson_feature debug symbol-lookup)

		$(meson_use gtk-doc gtk_doc)
	)

	meson_src_configure
}

multilib_src_test() {
	multilib_is_native_abi && meson_src_test
}

multilib_src_install_all() {
	einstalldocs

#;madhu 250710 docs on my system under /usr/share/gtk-doc/html/cairo
if false; then
	if use gtk-doc; then
		mkdir -p "${ED}"/usr/share/gtk-doc/html || die
		mv "${ED}"/usr/share/gtk-doc/{html/cairo,cairo/html} || die
		rmdir "${ED}"/usr/share/gtk-doc/html || die
	fi
fi

}

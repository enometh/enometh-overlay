# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Feb 05 00:02:55 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260205 qt5
#
EAPI=8

inherit meson

DESCRIPTION="Lightweight C library for loading and wrapping LV2 plugin UIs"
HOMEPAGE="https://drobilla.net/software/suil.html"
SRC_URI="https://download.drobilla.net/${P}.tar.xz"

LICENSE="ISC"
SLOT="0"
KEYWORDS="amd64 ~arm64 ~loong ~mips ppc ppc64 ~riscv x86"
IUSE="doc gtk gtk2 qt5  test X qt6"
RESTRICT="!test? ( test )"

# This could be way refined, but it's quickly a rabbit hole
# Take care on bumps to check lv2 minimum version!
RDEPEND="
	media-libs/lv2
	gtk2? (
		>=x11-libs/gtk+-2.18.0:2
		dev-libs/glib:2
	)
	gtk? (
		>=x11-libs/gtk+-3.14.0:3
		dev-libs/glib:2
	)
	qt5? (
		dev-qt/qtcore:5
		dev-qt/qtgui:5
		dev-qt/qtwidgets:5
		dev-qt/qtx11extras:5
	)
	qt6? ( dev-qt/qtbase:6[gui,widgets,X] )
	X? ( x11-libs/libX11 )
"
DEPEND="${RDEPEND}"
BDEPEND="
	virtual/pkgconfig
	doc? (
		app-text/doxygen
		dev-python/sphinx
		dev-python/sphinx-lv2-theme
		dev-python/sphinxygen
	)
	test? ( dev-libs/check )
"

DOCS=( AUTHORS NEWS README.md )

# PATCHES=( "${FILESDIR}/${P}-fix-gtk2-option.patch" )

src_prepare() {
	default

	# fix doc installation path
	sed -iE "s/versioned_name/'${PF}'/g" doc/html/meson.build doc/singlehtml/meson.build || die
}

src_configure() {
	local emesonargs=(
		$(meson_feature qt5)
		$(meson_feature doc docs)
		$(meson_feature gtk2)
		$(meson_feature gtk gtk3)
		$(meson_feature qt6)
		$(meson_feature test tests)
		$(meson_feature X x11)
	)

	meson_src_configure
}

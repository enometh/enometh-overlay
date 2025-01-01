# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Mar 24 17:18:39 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230324 2.6.1 -> 2.7.2
# ;madhu 230402 rebuild after using media-libs/sonivox network is
# ;required for drumstick-vpiano
# ;madhu 250101 2.10.0 - depend not on libpulse
EAPI=8

inherit cmake xdg

DESCRIPTION="Qt/C++ wrapper for ALSA sequencer"
HOMEPAGE="https://drumstick.sourceforge.io/"
SRC_URI="https://downloads.sourceforge.net/${PN}/${P}.tar.bz2"
# master.dl.sourceforge.net/project/drumstick/2.10.0/drumstick-2.10.0.tar.bz2?ts=

LICENSE="GPL-3+"
SLOT="0"
KEYWORDS="amd64 x86"
IUSE="alsa doc fluidsynth network +pulseaudio +pipewire"

RESTRICT="test"

BDEPEND="
	dev-libs/libxslt
	dev-qt/linguist-tools:5
	virtual/pkgconfig
	x11-misc/shared-mime-info
	doc? (
		app-text/doxygen[dot]
		app-text/docbook-xsl-stylesheets
	)

"
DEPEND="
	dev-qt/designer:5
	dev-qt/qtcore:5
	dev-qt/qtdbus:5
	dev-qt/qtgui:5
	dev-qt/qtsvg:5
	dev-qt/qtwidgets:5
	media-libs/alsa-lib
	fluidsynth? ( media-sound/fluidsynth )
	network? ( dev-qt/qtnetwork:5 )
	pulseaudio? ( media-sound/pulseaudio )
	pipewire? ( media-video/pipewire )
	media-libs/sonivox
"
RDEPEND="${DEPEND}"

DOCS=( AUTHORS ChangeLog NEWS readme.md TODO )

PATCHES=(
#	$FILESDIR/drumstick-2.7.2-allow-building-without-sonivox.patch
$FILESDIR/drumstick-2.10.0-build-with-qt-5.12.3.patch
$FILESDIR/drumstick-2.10.0-qt5.12-fix-floating-point-exceptions.patch
)

src_configure() {
	local mycmakeargs=(
		-DBUILD_RT=ON
		-DBUILD_TESTING=OFF
		-DUSE_DBUS=ON
		#;madhu 250101
		-DUSE_QT5=ON
		-DUSE_FLUIDSYNTH=$(usex fluidsynth)
		-DUSE_PIPEWIRE=$(usex pipewire)
		#;madhu 230324
		-DUSE_SONIVOX=ON
		-DUSE_NETWORK=$(usex network)
		-DUSE_PULSEAUDIO=$(usex pulseaudio)
		-DBUILD_DOCS=$(usex doc)
	)
	cmake_src_configure
}

src_compile() {
	cmake_src_compile
	use doc && cmake_src_compile doxygen
}

src_install() {
	use doc && local HTML_DOCS=( "${BUILD_DIR}"/doc/html/. )
	cmake_src_install
}

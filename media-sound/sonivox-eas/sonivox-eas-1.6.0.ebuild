# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Jan 01 23:58:51 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 230324 3.6.15 (WIP)
#
EAPI=8
inherit cmake xdg

HOMEPAGE="https://github.com/pedrolcl/Linux-SonivoxEas"
DESCRIPTION="Sonivox EAS for Linux and Qt"
SRC_URI="https://github.com/pedrolcl/Linux-SonivoxEas/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="~x86 ~amd64"
IUSE=""

S="${WORKDIR}/Linux-SonivoxEas-${PV}"

BDEPEND="
	dev-qt/linguist-tools:5
	virtual/pkgconfig
	media-sound/pulseaudio
	media-libs/sonivox
"
DEPEND="
	dev-qt/designer:5
	dev-qt/qtcore:5
	dev-qt/qtgui:5
	dev-qt/qtwidgets:5
	media-libs/alsa-lib
"

PATCHES=( $FILESDIR/Linux-SonivoxEas-1.6.0-don-t-fail-wipw-15.patch )

src_configure() {
	local mycmakeargs=(
		-DUSE_QT5=ON
	)
	cmake_src_configure
}

# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

#
#   Time-stamp: <>
#   Touched: Tue Mar 07 12:47:57 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230307 3.13-r1 -> 3.14

EAPI=8

inherit autotools optfeature xdg-utils

DESCRIPTION="Another free touch typing tutor program"
HOMEPAGE="https://klavaro.sourceforge.io/"
SRC_URI="mirror://sourceforge/project/${PN}/${P}.tar.bz2"

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="amd64 x86"

BDEPEND="
	dev-util/intltool
	>=sys-devel/gettext-0.18.3
"
RDEPEND="
	dev-libs/glib:2
	net-misc/curl
	x11-libs/gtk+:3
	>=x11-libs/gtkdatabox-1.0.0
	x11-libs/pango
"

DEPEND="${RDEPEND}"

PATCHES=(
	# https://sourceforge.net/p/klavaro/patches/16/
	"${FILESDIR}"/${PN}-3.13-datadir.patch
	# https://sourceforge.net/p/klavaro/patches/17/
	"${FILESDIR}"/${PN}-3.13-desktop-keywords.patch
)

src_prepare() {
	default

	eautoreconf
}

pkg_postinst() {
	xdg_icon_cache_update
	optfeature "instructions via synthesized speech" app-accessibility/espeak
}

pkg_postrm() {
	xdg_icon_cache_update
}

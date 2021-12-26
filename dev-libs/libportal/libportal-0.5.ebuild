# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2021-12-26 08:23:59 IST>
#   Touched: Mon Aug 03 17:37:02 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200803: 0.3
# ;madhu 211226: 0.5

EAPI=7

inherit meson

DESCRIPTION="Desktop integration portal"
HOMEPAGE="https://flatpak.org/ https://github.com/flatpak/${PN}"
SRC_URI="https://github.com/flatpak/${PN}/releases/download/${PV}/${P}.tar.xz"

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="amd64 ~arm arm64 ~ppc64 x86"
IUSE=""

BDEPEND="
	virtual/pkgconfig
"
DEPEND="
	dev-libs/glib:2
"
RDEPEND="${DEPEND}
"


src_configure() {
	local emesonargs=()
	meson_src_configure
}

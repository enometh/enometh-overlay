# Copyright 2024-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Aug 06 09:50:23 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250806 libsisocodes-1.2.5, no mulitlib because dev-libs/libgee is not multilib

EAPI=8

inherit vala # multilib-minimal

DESCRIPTION="Easily access XML data of the iso-codes package"
HOMEPAGE="https://github.com/toddy15/libisocodes"
SRC_URI="https://github.com/toddy15/${PN}/archive/refs/tags/v${PV}.tar.gz ->  ${P}.tar.gz"

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64"
IUSE="nls"

RDEPEND="
	nls? ( sys-devel/gettext )
	app-text/iso-codes
	>=dev-libs/libgee-0.8[introspection]
	dev-libs/libxml2
"
DEPEND="${RDEPEND}"
BDEPEND="
	dev-util/glib-utils
	$(vala_depend)
	virtual/pkgconfig
"

src_prepare() {
	default
	vala_setup
}

src_configure() {
	ECONF_SOURCE="${S}" econf $(use_enable nls)
}

src_install() {
	default
	find "${ED}" -name '*.la' -delete || die
}
# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Jan 18 16:20:23 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
#;madhu 220118 41.1

EAPI=7

inherit gnome.org meson systemd

MY_PV="${PV//_pre*}"
MY_P="${PN}-${MY_PV}"

DESCRIPTION="portal backend implementation that is using GTK+ and various pieces of GNOME"
HOMEPAGE="https://gitlab.gnome.org/GNOME/xdg-desktop-portal-gnome"

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="amd64 x86"
IUSE=""

BDEPEND="
	virtual/pkgconfig
"

DEPEND="
	dev-libs/glib:2
	sys-apps/dbus
	>=sys-apps/xdg-desktop-portal-1.7
	>=sys-apps/xdg-desktop-portal-gtk-1.7
"

RDEPEND="${DEPEND}"

S="${WORKDIR}/${MY_P}"

src_prepare() {
	sed -i -e "s/^\(subdir('po')\)/#1/g" meson.build
	default
}

src_configure() {
	local emesonargs=(
		-Dsystemduserunitdir="$(systemd_get_userunitdir)"
	)
	meson_src_configure
}

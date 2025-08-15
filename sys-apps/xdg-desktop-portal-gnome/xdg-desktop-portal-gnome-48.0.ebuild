# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Jan 18 16:20:23 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
#;madhu 220118 41.1
#;madhu 250815 48.0

EAPI=8

inherit gnome.org gnome2-utils meson systemd xdg

MY_PV="${PV//_pre*}"
MY_P="${PN}-${MY_PV}"

DESCRIPTION="Backend implementation for xdg-desktop-portal using GNOME"
HOMEPAGE="https://gitlab.gnome.org/GNOME/xdg-desktop-portal-gnome"

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~ppc ~ppc64 ~riscv ~x86"
IUSE="wayland X"

DEPEND="
	dev-libs/glib:2
	>=gnome-base/gsettings-desktop-schemas-47_alpha
	gnome-base/gnome-desktop:4=
	>=gui-libs/libadwaita-1.7_alpha:1
	media-libs/fontconfig
	sys-apps/dbus
	>=sys-apps/xdg-desktop-portal-1.19.1
	>=sys-apps/xdg-desktop-portal-gtk-1.14.0
	>=gui-libs/gtk-4.17.1:4[wayland?,X?]
	X? ( x11-libs/libX11 )
	wayland? ( dev-libs/wayland )
"
RDEPEND="${DEPEND}"
BDEPEND="
	dev-util/gdbus-codegen
	sys-devel/gettext
	virtual/pkgconfig

	wayland? ( dev-util/wayland-scanner )
"

# S="${WORKDIR}/${MY_P}"

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

pkg_postinst() {
	xdg_pkg_postinst
	gnome2_schemas_update
}

pkg_postrm() {
	xdg_pkg_postrm
	gnome2_schemas_update
}

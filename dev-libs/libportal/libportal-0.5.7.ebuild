# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Aug 03 17:37:02 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200803: 0.3
# ;madhu 211226: 0.5
# ;madhu 220118: 0.5.7 0.5-7-g257261c - handle portal-tests tests

EAPI=7
USE_GIT=true

inherit gnome2-utils meson vala xdg

DESCRIPTION="Desktop integration portal"
HOMEPAGE="https://flatpak.org/ https://github.com/flatpak/${PN}"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI=https://github.com/flatpak/libportal
	EGIT_CLONE_TYPE=shallow
else
SRC_URI="https://github.com/flatpak/${PN}/releases/download/${PV}/${P}.tar.xz"
fi

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="amd64 ~arm arm64 ~ppc64 x86"
IUSE="gtk-doc +introspection +vala"

BDEPEND="
	virtual/pkgconfig
	gtk-doc? (
		app-text/docbook-xml-dtd:4.3
		dev-util/gi-docgen
	)
"
DEPEND="
	vala? ( $(vala_depend) )
	dev-libs/glib:2
"
RDEPEND="${DEPEND}
"

src_prepare() {
	vala_src_prepare
	xdg_src_prepare
}

src_configure() {
	local backends=" gtk3 gtk4 qt5" #TODO

	local emesonargs=(
		-Dbackends="$(meson-format-array ${backends})"
		$(meson_use vala vapi)
		$(meson_use introspection)
		$(meson_use gtk-doc docs)
		-Dtests=false
	)
	meson_src_configure
}


src_install() {
	meson_src_install

	if use gtk-doc ; then
		mkdir -pv "${ED}"/usr/share/gtk-doc/html

		local docdirs=( libportal-1 )
		local d
		for d in "${docdirs[@]}"; do
			mv -v "${ED}"/usr/share/{doc/${d},gtk-doc/html/} || die
		done
	fi
}

pkg_postrm() {
	xdg_desktop_database_update
	xdg_icon_cache_update
}

pkg_postinst() {
	xdg_desktop_database_update
	xdg_icon_cache_update
}

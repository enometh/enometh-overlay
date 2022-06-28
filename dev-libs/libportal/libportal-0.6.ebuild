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
# ;madhu 220628: 0.6 0.5-36-gf77f892

EAPI=8
USE_GIT=true

inherit  meson vala virtualx

DESCRIPTION="Desktop integration portal"
HOMEPAGE="https://flatpak.org/ https://github.com/flatpak/${PN}"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI=https://github.com/flatpak/libportal
	EGIT_CLONE_TYPE=shallow
	EGIT_BRANCH=main
else
SRC_URI="https://github.com/flatpak/${PN}/releases/download/${PV}/${P}.tar.xz"
fi

LICENSE="LGPL-3"
SLOT="0" #"0/1-1-1-1" # soname of libportal{,-gtk3,-gtk4,-qt5}.so
KEYWORDS="amd64 ~arm arm64 ~ppc64 x86"
IUSE="gtk gtk-doc qt5 +introspection +vala"

REQUIRED_USE="
	gtk-doc? ( introspection )
	vala? ( introspection )
"

RDEPEND="
	>=dev-libs/glib-2.58:2
	introspection? ( dev-libs/gobject-introspection:= )
	gtk? (
		x11-libs/gtk+:3
		gui-libs/gtk:4
	)
	qt5? (
		dev-qt/qtcore:=
		dev-qt/qtgui:=
		dev-qt/qtx11extras:=
		dev-qt/qtwidgets:=
	)
"
DEPEND="${RDEPEND}"

BDEPEND="
	virtual/pkgconfig
	dev-util/glib-utils
	gtk-doc? (
		app-text/docbook-xml-dtd:4.3
		dev-util/gi-docgen
	)
	vala? ( $(vala_depend) )
"

src_prepare() {
	default
	vala_setup
}

src_configure() {
	local backends
	use gtk && backends+="gtk3,gtk4,"
	use qt5 && backends+="qt5,"

	local emesonargs=(
		-Dbackends=${backends%,}
		-Dportal-tests=false
		$(meson_use introspection)
		$(meson_use vala vapi)
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

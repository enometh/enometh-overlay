# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Aug 06 09:23:25 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 4.0.0. EXPERIMENTAL, should the package be named dev-libs/libzim-glib instead? bindings target version 7 but current version in 9

EAPI=8

USE_GIT=true

MY_COMMIT="1cd3e194b0ee923c3decfc527e27e30fe08fee08"

inherit meson vala

DESCRIPTION=" partial GObject/C bindings for libzim, created primarily to be used by WebArchives."
HOMEPAGE="https://github.com/birros/libzim-glib"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/birros/libzim-glib"
	if [ -n "$MY_COMMIT" ]; then
		EGIT_COMMIT="$MY_COMMIT"
	fi
	EGIT_CLONE_TYPE="shallow"
else
	die SRC_URI="https://github.com/birros/libzim-glib/${PN}/${P}.tar.gz"
fi

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""

RDEPEND="
	dev-libs/glib:2
	dev-libs/gobject-introspection:=
	app-arch/libzim
"

RDEPEND="$RDEPEND"
BDEPEND="
	dev-util/glib-utils
	$(vala_depend)
	virtual/pkgconfig
"

src_prepare() {
	default
	vala_setup
}

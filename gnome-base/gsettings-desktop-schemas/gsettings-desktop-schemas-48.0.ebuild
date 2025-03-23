# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Apr 25 17:03:57 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# bump up from 3.28.1 to 3.32.0 use meson
# ;madhu 190709 3.33.0
# ;madhu 190912 3.34.0
# ;madhu 200306 3.35.91 reintroduce font patch
# ;madhu 200722 3.37.2 git-r3
# ;madhu 210115 40_alpha 40.alpha-3-g2bd6d5a
# ;madhu 210805 41.alpha-5-geaa70f2
# ;madhu 220128 42.alpha-10-gc2f4b13
# ;madhu 220227 42.beta-2-g1c01168
# ;madhu 220324 42.0 42.0-1-g8633dc1
# ;madhu 221024 43.0 43.0-7-g723901e
# ;madhu 240215 46_beta 46.beta-4-g9c7a53e
# ;madhu 240926 47.1-1-ge06b358
# ;madhu 250119 48.alpha 48.alpha-2-gd48bc49
# ;madhu 250322 48.0 599bada

EAPI=8

USE_GIT=true

inherit gnome.org gnome2-utils meson xdg

DESCRIPTION="Collection of GSettings schemas for GNOME desktop"
HOMEPAGE="https://gitlab.gnome.org/GNOME/gsettings-desktop-schemas"

LICENSE="LGPL-2.1+"
SLOT="0"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~loong ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86 ~amd64-linux ~x86-linux ~x64-macos"
IUSE="+introspection"

if ${USE_GIT}; then
	inherit git-r3
	SRC_URI=""
	EGIT_REPO_URI="https://gitlab.gnome.org/GNOME/gsettings-desktop-schemas"
	S=${WORKDIR}/${P}
fi

BDEPEND="
	introspection? ( >=dev-libs/gobject-introspection-1.54:= )
	dev-util/glib-utils
	>=sys-devel/gettext-0.19.8
	virtual/pkgconfig
"

#if ! ${USE_GIT}; then
PATCHES=(
	# Revert change to 'Source Code Pro 10' and 'Cantarell 11' fonts back to generic sans and monospace aliases
	"${FILESDIR}"/48.0-default-fonts.patch
)
#fi

src_prepare() {
	default
	echo > po/LINGUAS
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build

#	python_fix_shebang build-aux/meson/post-install.py
}

src_configure() {
	local emesonargs=(
		$(meson_use introspection)
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

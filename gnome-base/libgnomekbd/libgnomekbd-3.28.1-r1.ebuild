# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Jul 06 20:21:00 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230706 3.28.1 3.28.1-11-gd510a0b, try to build gi-docgen pointing to local documentation. ;madhu 230710 gi-docgen sucks. patch to use gtk-doc. TODO: remove unsued autoconf gi-docgen stuff

EAPI=8
USE_GIT=true

inherit gnome.org meson

DESCRIPTION="Gnome keyboard configuration library"
HOMEPAGE="https://gitlab.gnome.org/GNOME/libgnomekbd"

if ${USE_GIT} ; then
	inherit git-r3
	SRC_URI=""
	EGIT_REPO_URI=https://gitlab.gnome.org/GNOME/libgnomekbd.git
	EGIT_CLONE_TYPE=shallow
	EGIT_BRANCH=master
	EGIT_COMMIT="d510a0bf7290471787dc8bbdaa0a1fe018846abf"
fi

LICENSE="LGPL-2+"
SLOT="0/8"
KEYWORDS="~alpha amd64 ~arm arm64 ~ia64 ~ppc ~ppc64 ~riscv ~sparc x86 ~amd64-linux ~x86-linux"
IUSE="+introspection gtk-doc"

RDEPEND="
	>=dev-libs/glib-2.44.0:2
	>=x11-libs/gtk+-2.91.7:3[X,introspection?]
	x11-libs/libX11
	>=x11-libs/libxklavier-5.2:=[introspection?]

	introspection? ( >=dev-libs/gobject-introspection-0.6.7:= )
"

#	gtk-doc? ( dev-util/gi-docgen )

DEPEND="${RDEPEND}"
BDEPEND="
	dev-util/glib-utils
	>=sys-devel/gettext-0.19.6
	virtual/pkgconfig
	gtk-doc? ( dev-util/gtk-doc )
"

PATCHES=(
 ${FILESDIR}/libgnomekbd-3.28-gtk-doc-integration.patch
 ${FILESDIR}/libgnomekbd-3.28-gtk-doc-meson.patch
)

src_prepare() {
	default
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
}

src_configure() {
	local emesonargs=(
		$(meson_use introspection)
		-Dvapi=false # XXX why not?
		-Dtests=false
		$(meson_use gtk-doc gtk_doc)
	)
	meson_src_configure
}

src_compile() {
	meson_src_compile

#  use gtk-doc instead of gi-docgen
#	gi-docgen generate --output-dir ${BUILD_DIR}/html --config ${FILESDIR}/generic.toml ${BUILD_DIR}/libgnomekbd/Gkbd-3.0.gir
}

src_install() {
	meson_src_install

# use gtk-doc instead of gi-docgen
#	mkdir -pv ${ED}/usr/share/gtk-doc
#	mv ${BUILD_DIR}/html  ${ED}/usr/share/gtk-doc
}

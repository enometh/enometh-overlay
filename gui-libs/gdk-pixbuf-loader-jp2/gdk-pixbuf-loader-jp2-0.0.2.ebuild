# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2021-12-25 17:57:47 IST>
#   Touched: Sun Dec 19 21:46:11 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 211219 0.0.2 0.0.2-10-g016faf4

EAPI=7

inherit gnome2-utils meson-multilib

USE_GIT=true

DESCRIPTION="WebP GDK Pixbuf Loader library"
HOMEPAGE="https://notabug.org/necklace/jp2-pixbuf-loader"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://notabug.org/necklace/jp2-pixbuf-loader"
S="${WORKDIR}/${P}"
else
SRC_URI="https://notabug.org/necklace/jp2-pixbuf-loader/archive/${PN}.tar.gz" -> "${P}.tar.gz"
S="${WORKDIR}/jp2-pixbuf-loader-${PV}" #untested
fi

LICENSE="LGPL-2.1+"
SLOT="0"
KEYWORDS="amd64 arm arm64 ppc ppc64 x86"
IUSE=""

RDEPEND=">x11-libs/gdk-pixbuf-2.22.0:2[${MULTILIB_USEDEP}]
	>media-libs/openjpeg-2.3.0:=[${MULTILIB_USEDEP}]"
DEPEND="${RDEPEND}"
BDEPEND="virtual/pkgconfig"

PATCHES=( ${FILESDIR}/gdk-pixbuf-loader-jp2-0.0.2-stub-support-for-incremental-loading-through-tmpfile.patch )

src_prepare() {
	default
	# Drop handling of pixbuf cache update by upstream
	sed -e '/query_loaders/d' -i meson.build || die
}

pkg_preinst() {
	gnome2_gdk_pixbuf_savelist
}

pkg_postinst() {
	# causes segfault if set, see bug 375615
	unset __GL_NO_DSO_FINALIZER
	multilib_foreach_abi gnome2_gdk_pixbuf_update
}

pkg_postrm() {
	# causes segfault if set, see bug 375615
	unset __GL_NO_DSO_FINALIZER
	multilib_foreach_abi gnome2_gdk_pixbuf_update
}

# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Apr 25 20:47:48 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190425 - bump from cogl-1.22.2.ebuild to cogl-1.22.4.ebuild
# kill LINGUAS. IUSE nls
# ;madhu 250424 1.22.8-r3 - fix gir generation for vapigen, reintroduce gtk-doc

EAPI=8

# Temporarily needed for slibtool patch
# It's upstreamed so should be able to drop in future
# bug #778041
GNOME2_EAUTORECONF="yes"
inherit flag-o-matic gnome2

DESCRIPTION="A library for using 3D graphics hardware to draw pretty pictures"
HOMEPAGE="https://www.cogl3d.org/"

LICENSE="MIT BSD"
SLOT="1.0/20" # subslot = .so version
KEYWORDS="~alpha amd64 ~arm arm64 ~loong ~mips ppc ppc64 ~riscv ~sparc x86"
# doc and profile disable for now due to bugs #484750 and #483332
# reenable doc
IUSE="debug doc examples gles2 gstreamer +introspection +kms nls +opengl +pango test wayland doc" # doc profile
REQUIRED_USE="
	wayland? ( gles2 )
	|| ( gles2 opengl )
"
# Need classic mesa swrast for tests, llvmpipe causes a test failure
# For some reason GL3 conformance test all fails again...
RESTRICT="test"

# 	gles2? ( media-libs/libglvnd )
#	gles2? ( media-libs/mesa[gles2] )
DEPEND="
	>=dev-libs/glib-2.32:2
	x11-libs/cairo:=
	>=x11-libs/gdk-pixbuf-2:2
	x11-libs/libX11
	>=x11-libs/libXcomposite-0.4
	x11-libs/libXdamage
	x11-libs/libXext
	>=x11-libs/libXfixes-3
	>=x11-libs/libXrandr-1.2
	virtual/opengl
	gstreamer? (
		media-libs/gstreamer:1.0
		media-libs/gst-plugins-base:1.0
	)
	introspection? ( >=dev-libs/gobject-introspection-1.34.2:= )
	kms? (
		media-libs/mesa[gbm(+)]
		x11-libs/libdrm:=
	)
	pango? ( >=x11-libs/pango-1.20.0[introspection?] )
	wayland? (
		>=dev-libs/wayland-1.1.90
		media-libs/mesa[wayland]
	)
"
RDEPEND="${DEPEND}"
BDEPEND="
	dev-util/glib-utils
	>=sys-devel/gettext-0.19
	virtual/pkgconfig
"

PATCHES=(
	"${FILESDIR}"/${P}-slibtool.patch
	$FILESDIR/cogl-1.22.8--cogl-headers-avoid-typedef-void-Class.patch
	$FILESDIR/cogl-1.22.8--cogl-gst-Makefile.am-fix-gst_init.patch
)

src_prepare() {
#madhu kill LINGUAS
	echo > po/LINGUAS

	# Do not build examples
	sed -e "s/^\(SUBDIRS +=.*\)examples\(.*\)$/\1\2/" \
		-i Makefile.am Makefile.in || die

	if ! use test ; then
	# For some reason the configure switch will not completely disable
	# tests being built
	sed -e "s/^\(SUBDIRS =.*\)test-fixtures\(.*\)$/\1\2/" \
		-e "s/^\(SUBDIRS +=.*\)tests\(.*\)$/\1\2/" \
		-e "s/^\(.*am__append.* \)tests\(.*\)$/\1\2/" \
		-i Makefile.am Makefile.in || die
	fi

	gnome2_src_prepare
}

src_configure() {
	# bug #943759
	append-cflags -std=gnu17

	# TODO: think about quartz, sdl
	# Prefer gl over gles2 if both are selected
	# Profiling needs uprof, which is not available in portage yet, bug #484750
	# FIXME: Doesn't provide prebuilt docs, but they can neither be rebuilt, bug #483332
	gnome2_src_configure \
		--disable-examples-install \
		--disable-maintainer-flags \
		--enable-cairo             \
		--enable-deprecated        \
		--enable-gdk-pixbuf        \
		--enable-glib              \
		$(use_enable doc gtk-doc)  \
		$(use_enable nls)          \
		$(use_enable debug)        \
		$(use_enable opengl glx)   \
		$(use_enable opengl gl)    \
		$(use_enable gles2)        \
		$(use_enable gles2 cogl-gles2) \
		$(use_enable gles2 xlib-egl-platform) \
		$(usev gles2 --with-default-driver=$(usex opengl gl gles2)) \
		$(use_enable gstreamer cogl-gst)    \
		$(use_enable introspection) \
		$(use_enable kms kms-egl-platform) \
		$(use_enable pango cogl-pango) \
		$(use_enable test unit-tests) \
		$(use_enable wayland wayland-egl-platform) \
		$(use_enable wayland wayland-egl-server) \
		--disable-profile
}

src_install() {
	if use examples; then
		docinto examples
		dodoc examples/{*.c,*.jpg}
		docompress -x /usr/share/doc/${PF}/examples
	fi

	gnome2_src_install

	# Remove silly examples-data directory
	rm -rvf "${ED}"/usr/share/cogl/examples-data/ || die
}

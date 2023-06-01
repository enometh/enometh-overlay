# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Jun 30 16:13:15 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210630 3.40.1-r1 (addpython3_7) upstream dropped python2_7
# ;madhu 220428 3.42.1 (40.0)
# ;madhu 230601 3.42.1-r46 (slot 46) for py_3[78], drop pkgconfig,
#     usr/include, examples

EAPI=7

DISTUTILS_EXT=1
DISTUTILS_USE_PEP517=no
PYTHON_COMPAT=( python3_{7..8} )

inherit gnome.org meson python-r1 virtualx xdg

DESCRIPTION="Python bindings for GObject Introspection"
HOMEPAGE="https://pygobject.readthedocs.io/"

LICENSE="LGPL-2.1+"
SLOT="46"
KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~sparc-solaris ~x64-solaris ~x86-solaris"
IUSE="+cairo examples test"
RESTRICT="test"
REQUIRED_USE="${PYTHON_REQUIRED_USE}"

RDEPEND="${PYTHON_DEPS}
	>=dev-libs/glib-2.56:2
	>=dev-libs/gobject-introspection-1.56:=
	dev-libs/libffi:=
	cairo? (
		>=dev-python/pycairo-1.16.0[${PYTHON_USEDEP}]
		x11-libs/cairo[glib] )
"
DEPEND="${RDEPEND}
	test? (
		dev-libs/atk[introspection]
		dev-python/pytest[${PYTHON_USEDEP}]
		x11-libs/gdk-pixbuf:2[introspection,jpeg]
		x11-libs/gtk+:3[introspection]
		x11-libs/pango[introspection]
	)
"
BDEPEND="
	virtual/pkgconfig
"

PATCHES=(
)

src_configure() {
	configuring() {
		meson_src_configure \
			$(meson_feature cairo pycairo) \
			$(meson_use test tests) \
			-Dpython="${EPYTHON}"
	}

	python_foreach_impl configuring
}

src_compile() {
	python_foreach_impl meson_src_compile
}

src_test() {
	local -x GIO_USE_VFS="local" # prevents odd issues with deleting ${T}/.gvfs
	local -x GIO_USE_VOLUME_MONITOR="unix" # prevent udisks-related failures in chroots, bug #449484

	testing() {
		local -x XDG_CACHE_HOME="${T}/${EPYTHON}"
		meson_src_test --timeout-multiplier 3 || die "test failed for ${EPYTHON}"
	}
	virtx python_foreach_impl testing
}

src_install() {
	installing() {
		meson_src_install
		python_optimize
	}
	python_foreach_impl installing
	if [ ! $SLOT="46" ]; then
	use examples && dodoc -r examples
	else
		rm -fv ${ED}/usr/$(get_libdir)/pkgconfig/pygobject-3.0.pc
		rm -fv ${ED}/usr/include/pygobject-3.0/pygobject.h
	fi
}

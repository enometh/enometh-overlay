# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sat Mar 13 08:48:27 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210313 2.50.3 - slot 2.50
# ;madhu 220508 2.54.0->2.54.1 - fake installed i686 toolchain, go back to slot 0
# ;madhu 250414 2.58.5 use cargo vendor-filterer --platform=x86_64-unknown-linux-gnu > ../config.toml and dump {PV}-vendor1.tar.xz from the vendor directory
# ;madhu 250710 2.58.5-r1 USE="-abi_x86_32" only, can't deal

EAPI=8
PYTHON_COMPAT=( python3_{8..10} )
CARGO_OPTIONAL=1
CRATES=""

MULTILIB_ABIS="amd64"

inherit cargo gnome2 multilib-minimal python-any-r1 rust-toolchain vala

DESCRIPTION="Scalable Vector Graphics (SVG) rendering library"
HOMEPAGE="https://wiki.gnome.org/Projects/LibRsvg https://gitlab.gnome.org/GNOME/librsvg"
SRC_URI+=" ${CARGO_CRATE_URIS}"
SRC_URI+="http://example.com/librsvg-2.58.5-vendor1.tar.xz"

LICENSE="LGPL-2.1+"
# Dependent crate licenses
LICENSE+="
	Apache-2.0 Apache-2.0-with-LLVM-exceptions BSD ISC MIT MPL-2.0
	Unicode-DFS-2016
"
SLOT="2.50"
#SLOT="0"
KEYWORDS="amd64 arm arm64 ~loong ~mips ppc ppc64 ~riscv ~s390 sparc x86"

IUSE="gtk-doc +introspection +vala"
REQUIRED_USE="
	gtk-doc? ( introspection )
	vala? ( introspection )
"

RDEPEND="
	>=x11-libs/cairo-1.17.0[glib,svg(+),${MULTILIB_USEDEP}]
	>=media-libs/freetype-2.9:2[${MULTILIB_USEDEP}]
	>=x11-libs/gdk-pixbuf-2.20:2[introspection?,${MULTILIB_USEDEP}]
	>=dev-libs/glib-2.50.0:2[${MULTILIB_USEDEP}]
	>=media-libs/harfbuzz-2.0.0:=[${MULTILIB_USEDEP}]
	>=dev-libs/libxml2-2.9.1-r4:2=[${MULTILIB_USEDEP}]
	>=x11-libs/pango-1.50.0[${MULTILIB_USEDEP}]

	introspection? ( >=dev-libs/gobject-introspection-0.10.8:= )
"
DEPEND="${RDEPEND}"
BDEPEND="
	x11-libs/gdk-pixbuf
	${PYTHON_DEPS}
	$(python_gen_any_dep 'dev-python/docutils[${PYTHON_USEDEP}]')
	gtk-doc? ( dev-util/gi-docgen )
	virtual/pkgconfig
	vala? ( $(vala_depend) )
"
# >=gtk-doc-am-1.13, gobject-introspection-common, vala-common needed by eautoreconf

QA_FLAGS_IGNORED="
	usr/bin/rsvg-convert
	usr/lib.*/librsvg.*
"

RESTRICT="test" # Lots of issues on 32bit builds, 64bit build seems to get into an infinite compilation sometimes, etc.

src_unpack() {
	default_src_unpack
	# will unpack venddor directory to vendor1/
	# don't call cargo_src_unpack but at least call
	cargo_gen_config
	ln -sv ../vendor1 cargo_home/gentoo || die

}

src_prepare() {
	# Documentation is built unconditionally and depends on introspection,
	# but introspection is only built for the primary ABI.
	# Disable documentation and manually build the doc subdirectory separately.
#;madhu 220508	sed -i -e '/SUBDIRS =/s/ doc//' Makefile.in Makefile.am || die
#	sed -i -e 's|\(RSVG_DOC =\).*|\1|g' Makefile.in Makefile.am || die
	use vala && vala_setup # adapt to agenda: vala_src_prepare removed in EAPI 8
	gnome2_src_prepare

	export CARGO=/opt/rust/bin/cargo RUSTC=/opt/rust/bin/rustc
}

src_configure() {
	multilib-minimal_src_configure
}

multilib_src_configure() {
	local myconf=(
		--disable-static
		--disable-debug
		$(multilib_native_use_enable gtk-doc)
		$(multilib_native_use_enable introspection)
		$(multilib_native_use_enable vala)
		--enable-pixbuf-loader
	)

	if ! multilib_is_native_abi; then
		myconf+=(
			# Set the rust target, which can differ from CHOST
			RUST_TARGET="$(rust_abi)"
			# RUST_TARGET is only honored if cross_compiling, but non-native ABIs aren't cross as
			# far as C parts and configure auto-detection are concerned as CHOST equals CBUILD
			cross_compiling=yes
		)
	fi

	ECONF_SOURCE=${S} \
	gnome2_src_configure "${myconf[@]}"

	#;madhu 250710 - this looks bogus
	if multilib_is_native_abi; then
		ln -sv "${S}"/doc/html doc/html
	fi
}

src_compile() {
	multilib-minimal_src_compile
}

multilib_src_compile() {
	cargo_env gnome2_src_compile

# if RSVG_DOC = ""
#	if multilib_is_native_abi && use introspection; then
#		emake -C doc
#	fi
}

 multilib_src_test() {
	cargo_env default
}

src_install() {
	multilib-minimal_src_install
}

multilib_src_install() {
	cargo_env gnome2_src_install

# if RSVG_DOC = ""
#	if multilib_is_native_abi && use introspection; then
#		emake DESTDIR="${D}" install -C doc
#	fi

}

multilib_src_install_all() {
	find "${ED}" -name '*.la' -delete || die
	rm -fv ${ED}/usr/share/doc/${PF}/code-of-conduct.md || die

	if use gtk-doc; then
		mkdir -p "${ED}"/usr/share/gtk-doc/html/ || die
#		mv "${ED}"/usr/share/doc/${PF}/Rsvg-2.0 "${ED}"/usr/share/gtk-doc/html/ || die
		mv "${ED}"/usr/share/doc/Rsvg-2.0 "${ED}"/usr/share/gtk-doc/html/ || die
	fi
}

pkg_postinst() {
	multilib_foreach_abi gnome2_pkg_postinst
}

pkg_postrm() {
	multilib_foreach_abi gnome2_pkg_postrm
}

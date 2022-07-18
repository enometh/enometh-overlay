# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

#
#   Time-stamp: <>
#   Touched: Tue Jan 19 18:45:45 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021-2022 Madhu.  All Rights Reserved.
#
# ;madhu 210119 3.38.0 -> 3.38.1
# ;madhu 220718 3.40.0 -> 3.40.1 IUSE ssh-agent to avoid libsecret

EAPI=8
PYTHON_COMPAT=( python3_{8..10} )

inherit gnome.org gnome2-utils meson python-any-r1 vala xdg

DESCRIPTION="Libraries for cryptographic UIs and accessing PKCS#11 modules"
HOMEPAGE="https://gitlab.gnome.org/GNOME/gcr"

LICENSE="GPL-2+ LGPL-2+"
SLOT="0/1" # subslot = suffix of libgcr-base-3 and co

IUSE="gtk gtk-doc +introspection systemd test +vala  ssh-agent"
REQUIRED_USE="vala? ( introspection )"
RESTRICT="!test? ( test )"

KEYWORDS="~alpha amd64 ~arm ~arm64 ~ia64 ~loong ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86 ~amd64-linux ~x86-linux ~sparc-solaris ~x86-solaris"

DEPEND="
	>=dev-libs/glib-2.44.0:2
	>=dev-libs/libgcrypt-1.2.2:0=
	>=app-crypt/p11-kit-0.19.0
	ssh-agent? ( >=app-crypt/libsecret-0.20 )
	systemd? ( sys-apps/systemd:= )
	gtk? ( >=x11-libs/gtk+-3.22:3[introspection?] )
	>=sys-apps/dbus-1
	introspection? ( >=dev-libs/gobject-introspection-1.58:= )
"
RDEPEND="${DEPEND}"
PDEPEND="app-crypt/gnupg"
BDEPEND="
	${PYTHON_DEPS}
	gtk? ( dev-libs/libxml2:2 )
	dev-util/gdbus-codegen
	dev-util/glib-utils
	gtk-doc? (
		>=dev-util/gtk-doc-1.9
		app-text/docbook-xml-dtd:4.1.2
	)
	>=sys-devel/gettext-0.19.8
	test? ( app-crypt/gnupg )
	virtual/pkgconfig
	vala? ( $(vala_depend) )
"

PATCHES=(
	"${FILESDIR}"/3.38.1-optional-vapi.patch
)

pkg_setup() {
	python-any-r1_pkg_setup
}

src_prepare() {
	default
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	use vala && vala_setup
	xdg_environment_reset
}

src_configure() {
	local emesonargs=(
		$(meson_use introspection)
		$(meson_use gtk)
		$(meson_use gtk-doc gtk_doc)
		-Dgpg_path="${EPREFIX}"/usr/bin/gpg
		$(meson_use ssh-agent ssh_agent)
		$(meson_feature systemd)
		$(meson_use vala vapi)
	)
	meson_src_configure
}

src_install() {
	meson_src_install
	mkdir -pv ${ED}/usr/share/gtk-doc/html
	mv -iv ${ED}/usr/share/doc/* ${ED}/usr/share/gtk-doc/html
}

src_test() {
	dbus-run-session meson test -C "${BUILD_DIR}" || die 'tests failed'
}

pkg_postinst() {
	xdg_pkg_postinst
	gnome2_schemas_update
}

pkg_postrm() {
	xdg_pkg_postrm
	gnome2_schemas_update
}

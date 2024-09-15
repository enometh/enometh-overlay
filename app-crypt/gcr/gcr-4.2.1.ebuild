# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Oct 24 01:05:06 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 221024 4.0.0 slot 0 ssh-agent optional, delete conflicting
# ;files here instead of in gcr-3.41.1
# ;madhu 240915 4.2.1

EAPI=8

inherit flag-o-matic gnome.org gnome2-utils meson vala xdg

DESCRIPTION="Libraries for cryptographic UIs and accessing PKCS#11 modules"
HOMEPAGE="https://gitlab.gnome.org/GNOME/gcr"

LICENSE="GPL-2+ LGPL-2+"
SLOT="4/gcr-4.4-gck-2.2" # subslot = soname and soversion of libgcr and libgck

IUSE="gtk gtk-doc +introspection systemd test +vala ssh-agent"
REQUIRED_USE="
	gtk-doc? ( introspection )
	vala? ( introspection )
"
RESTRICT="!test? ( test )"

KEYWORDS="~alpha amd64 arm arm64 ~ia64 ~loong ~mips ppc ppc64 ~riscv sparc x86 ~amd64-linux ~x86-linux"

DEPEND="
	>=dev-libs/glib-2.68.0:2
	>=dev-libs/libgcrypt-1.2.2:0=
	>=app-crypt/p11-kit-0.19.0
	ssh-agent? ( >=app-crypt/libsecret-0.20 )
	systemd? ( sys-apps/systemd:= )
	gtk? ( gui-libs/gtk:4[introspection?] )
	>=sys-apps/dbus-1
	introspection? ( >=dev-libs/gobject-introspection-1.58:= )
	!<app-crypt/gcr-3.41.1
"
# 	!<app-crypt/gcr-3.41.1-r2

RDEPEND="${DEPEND}"
PDEPEND="app-crypt/gnupg"
BDEPEND="
	gtk? ( dev-libs/libxml2:2 )
	dev-util/gdbus-codegen
	dev-util/glib-utils
	gtk-doc? ( dev-util/gi-docgen )
	>=sys-devel/gettext-0.19.8
	test? ( app-crypt/gnupg )
	virtual/pkgconfig
	vala? ( $(vala_depend) )
"

src_prepare() {
	default
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	use vala && vala_setup
	xdg_environment_reset
}

src_configure() {
	filter-lto # https://gitlab.gnome.org/GNOME/gcr/-/issues/43
	local emesonargs=(
		$(meson_use introspection)
		$(meson_use gtk gtk4)
		$(meson_use gtk-doc gtk_doc)
		-Dgpg_path="${EPREFIX}"/usr/bin/gpg
		$(meson_use ssh-agent ssh_agent)
		$(meson_feature systemd)
		$(meson_use vala vapi)
	)
	meson_src_configure
}

src_test() {
	dbus-run-session meson test -C "${BUILD_DIR}" || die 'tests failed'
}

src_install() {
	meson_src_install

	# These files are installed by gcr:0
	local conflicts=()
	use ssh-agent && conflicts+=(
		"${ED}"/usr/libexec/gcr-ssh-agent
	)
	use systemd && conflicts+=(
		"${ED}"/usr/lib/systemd/user/gcr-ssh-agent.{service,socket}
	)
	einfo "${conflicts[@]}"
	rm -v "${conflicts[@]}" || die

	if use gtk-doc; then
		mkdir -p "${ED}"/usr/share/gtk-doc/html/ || die
		mv -v "${ED}"/usr/share/doc/{gck-2,gcr-4} "${ED}"/usr/share/gtk-doc/html/ || die
	fi
}

pkg_postinst() {
	xdg_pkg_postinst
	gnome2_schemas_update
}

pkg_postrm() {
	xdg_pkg_postrm
	gnome2_schemas_update
}

# Copyright 2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Mar 24 21:34:35 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220324 1.1.0 1.1.0-2-gee83dd7 - use locally installed gi-gendoc to avoid polluting the filesystem with the ugly shipped fonts

EAPI=7

USE_GIT=true

PYTHON_COMPAT=( python3_{8..10} )
VALA_MIN_API_VERSION="0.52"
inherit gnome.org meson python-any-r1 vala virtualx

DESCRIPTION="Building blocks for modern adaptive GNOME applications"
HOMEPAGE="https://gnome.pages.gitlab.gnome.org/libadwaita/ https://gitlab.gnome.org/GNOME/libadwaita"

LICENSE="LGPL-2.1+"
SLOT="1"
IUSE="+introspection test +vala +gtk-doc examples"
REQUIRED_USE="vala? ( introspection )"

if ${USE_GIT}; then
	SRC_URI=""
	inherit git-r3
	EGIT_REPO_URI=https://gitlab.gnome.org/GNOME/libadwaita.git
	EGIT_BRANCH=main
	EGIT_CLONE_TYPE=shallow
	EGIT_SUBMODULES=()
fi

KEYWORDS="~amd64 ~arm ~arm64 ~ppc64 ~riscv ~x86"

DEPEND="
	>=dev-libs/glib-2.66:2
	>=gui-libs/gtk-4.5.0:4[introspection?]
	dev-libs/fribidi
	introspection? ( >=dev-libs/gobject-introspection-1.54:= )
"
RDEPEND="${DEPEND}"
BDEPEND="
	${PYTHON_DEPS}
	vala? ( $(vala_depend) )
	>=dev-util/meson-0.59.0
	dev-util/glib-utils
	sys-devel/gettext
	virtual/pkgconfig
"

if ${USE_GIT}; then
BDEPEND+="
	gtk-doc? (
		>=dev-util/gi-docgen-2021.6
		app-text/docbook-xml-dtd:4.3
	)
"
fi

src_prepare() {
	echo > po/LINGUAS
	use vala && vala_src_prepare
	default
}

src_configure() {
	local emesonargs=(
		# Never use gi-docgen subproject
		--wrap-mode nofallback

		-Dprofiling=false
		$(meson_feature introspection)
		$(meson_use vala vapi)
		$(meson_use test tests)
		$(meson_use examples)
	)

	if ${USE_GIT}; then
		local emesonargs+=(
			$(meson_use gtk-doc gtk_doc)
		)
	fi

	meson_src_configure
}

src_test() {
	virtx meson_src_test --timeout-multiplier 2
}

src_install() {
	meson_src_install

	if use gtk-doc; then
		if ! ${USE_GIT}; then
			insinto ${ED}/usr/share/gtk-doc/html
			# This will install libadwaita API docs unconditionally, but this is intentional
			doins -r "${S}"/doc/libadwaita-1
		else
			mv ${ED}/usr/share/{doc,gtk-doc/html}/libadwaita-1
		fi
	fi
}

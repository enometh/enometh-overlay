# Copyright 2022-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Mar 24 21:34:35 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 220324 1.1.0 1.1.0-2-gee83dd7 - use locally installed gi-gendoc to avoid polluting the filesystem with the ugly shipped fonts
# ;madhu 221023 1.2.0 1.2.0-45-gf6a5dea TODO:  call xdg_icon_cache_update() in pkg_postinst() and pkg_postrm() - checkout the specific commit
# ;madhu 230517 1.3.2 (skip 1.3.rc-188-g0c154e2, build from tarball)
# ;madhu 230517 1.3.2-r1 bc77eca4ee7d4d72 1.3.rc-144-g96a28b6 for 1.4alpha, use -examples without appstream

EAPI=8

PYTHON_COMPAT=( python3_{9..11} )
inherit gnome.org meson python-any-r1 vala virtualx
USE_GIT=true

MY_COMMIT=96a28b62d189b79b4f2118b8e2ba40809d1cc914

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
	EGIT_COMMIT=$MY_COMMIT
	EGIT_CLONE_TYPE=shallow
	EGIT_SUBMODULES=()
fi

KEYWORDS="~amd64 ~arm ~arm64 ~ia64 ~loong ~ppc ~ppc64 ~riscv ~sparc ~x86"

RDEPEND="
	>=dev-libs/glib-2.72:2
	>=gui-libs/gtk-4.9.5:4[introspection?]
	dev-libs/fribidi
	introspection? ( >=dev-libs/gobject-introspection-1.54:= )
"
DEPEND="${RDEPEND}"
BDEPEND="
	${PYTHON_DEPS}
	vala? ( $(vala_depend) )
	dev-util/glib-utils
	sys-devel/gettext
	virtual/pkgconfig
	examples? ( dev-libs/appstream-glib )
	test? ( dev-libs/appstream-glib )
"

src_prepare() {
	echo > po/LINGUAS
	default
	use vala && vala_setup
}

src_configure() {
	local emesonargs=(
		# Never use gi-docgen subproject
		--wrap-mode nofallback

		-Dprofiling=false
		$(meson_feature introspection)
		$(meson_use vala vapi)
#		-Dgtk_doc=false # we ship pregenerated docs
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
			mkdir -pv ${ED}/usr/share/gtk-doc/html
			mv -v ${ED}/usr/share/{doc,gtk-doc/html}/libadwaita-1
		fi
	fi
}

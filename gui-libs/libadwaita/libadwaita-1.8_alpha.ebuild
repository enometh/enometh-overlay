# Copyright 2022-2025 Gentoo Authors
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
# ;madhu 231210 1.4.0-r1 (1.5alpha) 1.4.0-33-g42c04e0
# ;madhu 240215 1.5_beta (1.5.beta-7-ge8f1582)
# ;madhu 240725 1.6_alpha (1.6.alpha-38-g3441dca5)
# ;madhu 250710 1.8.alpha (1.8-g6d6a4b98)
EAPI=8

PYTHON_COMPAT=( python3_{11..14} )
inherit gnome.org meson python-any-r1 vala virtualx gnome-versioning
USE_GIT=true

MY_COMMIT=6d6a4b98144cc0f8a71b28d8ed315599dba9193f

DESCRIPTION="Building blocks for modern adaptive GNOME applications"
HOMEPAGE="https://gnome.pages.gitlab.gnome.org/libadwaita/ https://gitlab.gnome.org/GNOME/libadwaita"

LICENSE="LGPL-2.1+"
SLOT="1"
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~ppc ~ppc64 ~riscv ~x86"

if ${USE_GIT}; then
	SRC_URI=""
	inherit git-r3
	EGIT_REPO_URI=https://gitlab.gnome.org/GNOME/libadwaita.git
	EGIT_BRANCH=main
	EGIT_COMMIT=$MY_COMMIT
	EGIT_CLONE_TYPE=shallow
	EGIT_SUBMODULES=()
	S=${WORKDIR}/${P}
fi

IUSE="+introspection test +vala +gtk-doc examples"
REQUIRED_USE="vala? ( introspection )"

RDEPEND="
	>=dev-libs/glib-2.80.0:2
	>=gui-libs/gtk-4.17.5:4[introspection?]
	dev-libs/appstream:=
	dev-libs/fribidi
	introspection? ( >=dev-libs/gobject-introspection-1.83.2:= )
"
DEPEND="${RDEPEND}
	x11-base/xorg-proto"
BDEPEND="
	${PYTHON_DEPS}
	vala? ( $(vala_depend) )
	dev-util/glib-utils
	sys-devel/gettext
	virtual/pkgconfig
	dev-lang/sassc
"

src_prepare() {
	sed -i -e "s/^subdir('po')/#&/g" meson.build
	echo > po/LINGUAS
	default
	use vala && vala_setup
	xdg_environment_reset
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
			$(meson_use gtk-doc documentation)
		)
	fi

	meson_src_configure
}

src_test() {
	addwrite /dev/dri
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

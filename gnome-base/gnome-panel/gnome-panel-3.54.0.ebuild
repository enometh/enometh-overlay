# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

#   Time-stamp: <>
#   Touched: Sat May 11 08:53:09 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# taken from https://sources.gentoo.org/cgi-bin/viewvc.cgi/gentoo-x86/gnome-base/gnome-panel/gnome-panel-3.8.0.ebuild?revision=1.2
#
# ;madhu 190511 bump EAPI 6
# IUSE gweather - drop  clock.
#TODO: fix optional elogind
#
# drop dependencies on polkit telepathy
# upstream dropped gconf network-manager introspection
#
# 3.32.0-r1 ;madhu 190722 - bring back gdm, polkit, clock. just drop
# gweather
# ;madhu 190726 add assert patch
# ;madhu 190912 -3.34.0 move
# ;madhu 200228 3.35.2
# ;madhu 200805 3.37.1
# ;madhu 201230 3.38.0
# ;madhu 210630 3.40.0
# ;madhu 221112 3.46.0
# ;madhu 241217 3.54.0, fix typo for menu_modules in src_configure, add gnome-dekstop-3.0 to reqs

EAPI=7
inherit gnome2 toolchain-funcs autotools
USE_GIT=false

DESCRIPTION="The GNOME Flashback Panel"
HOMEPAGE="https://gitlab.gnome.org/GNOME/gnome-panel/"

LICENSE="GPL-2+ FDL-1.1 LGPL-2.1+"
SLOT="0"
KEYWORDS="~amd64 ~riscv"
IUSE="weather eds elogind systemd nls"
REQUIRED_USE="^^ ( elogind systemd )"
KEYWORDS="~amd64 ~riscv"

if ${USE_GIT} ; then
	inherit git-r3
	SRC_URI=""
	EGIT_REPO_URI="https://gitlab.gnome.org/GNOME/gnome-panel.git"
	EGIT_BRANCH=madhu-3.54.0
	EGIT_SUBMODULES=()
fi

RDEPEND="
	>=gnome-base/gnome-desktop-3.53.3:3=
	>=x11-libs/gdk-pixbuf-2.26.0:2
	>=x11-libs/pango-1.15.4
	>=dev-libs/glib-2.67.1:2
	>=x11-libs/gtk+-3.22.0:3[X]
	>=x11-libs/libwnck-43.0:3
	>=gnome-base/gnome-menus-3.7.90:3
	>=gnome-base/gsettings-desktop-schemas-42.0
	eds? ( >=gnome-extra/evolution-data-server-3.46.0:= )
	elogind? ( >=sys-auth/elogind-230 )
	systemd? ( >=sys-apps/systemd-230:= )
	>=x11-libs/cairo-1.0.0[X,glib]
	weather? ( >=dev-libs/libgweather-4.2.0:4= )
	>=gnome-base/dconf-0.13.4
	>=x11-libs/libXrandr-1.3.0
	gnome-base/gdm
	x11-libs/libX11
	x11-libs/libXi
	sci-geosciences/geocode-glib:2
	sys-auth/polkit
"
DEPEND="${RDEPEND}
	x11-base/xorg-proto
"
BDEPEND="
	app-text/docbook-xml-dtd:4.1.2
	dev-util/gdbus-codegen
	dev-util/glib-utils
	dev-util/itstool
	>=sys-devel/gettext-0.19.8
	virtual/pkgconfig
" # yelp-tools and autoconf-archive for eautoreconf

if $USE_GIT; then
# git patches configure.ac to use elogind
src_prepare() {
	default
	eautoreconf
}
fi

src_configure() {
	local myconf=(
		--disable-static
		$(use_enable eds)
	)

	# Below elogind MENU_* pkg-config calls need to match up with what upstream has
	# each version (libsystemd replaced with libelogind). Explicit per-version die
	# to force a manual recheck. Only update the explicit version if the
	# "PKG_CHECK_MODULES([MENU], ...)" block did not change; otherwise adjust
	# elogind conditional block below accordingly first.
	# DO NOT just change the version, look in configure.ac in which PKG_CHECK_MODULES-sections
	# libsystemd is used and check if there are new sections where it is used!
#	if ver_test ${PV} -ne 3.46.0; then
#		die "Maintainer has not checked over packages MENU pkg-config deps for elogind support"
#	fi

	if use elogind; then
		local pkgconfig="$(tc-getPKG_CONFIG)"

		local common_modules="gio-unix-2.0 gtk+-3.0 libgnome-menu-3.0  gnome-desktop-3.0"
		local action_modules="$common_modules libelogind x11"
		local launcher_modules="$common_modules libelogind"
		local menu_modules="$common_modules libelogind"

		myconf+=(
			ACTION_BUTTON_CFLAGS="$(${pkgconfig} --cflags ${action_modules})"
			ACTION_BUTTON_LIBS="$(${pkgconfig} --libs ${action_modules})"
			LAUNCHER_CFLAGS="$(${pkgconfig} --cflags ${launcher_modules})"
			LAUNCHER_LIBS="$(${pkgconfig} --libs ${launcher_modules})"
			MENU_CFLAGS="$(${pkgconfig} --cflags ${menu_modules})"
			MENU_LIBS="$(${pkgconfig} --libs ${menu_modules})"
		)
	fi
	DOCS="AUTHORS ChangeLog HACKING NEWS README.md"
	gnome2_src_configure "${myconf[@]}"
}

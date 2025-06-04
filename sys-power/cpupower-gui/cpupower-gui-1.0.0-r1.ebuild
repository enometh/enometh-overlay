# Copyright 2020-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Dec 15 15:57:57 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 201215 1.0.0 v1.0.0-2-ga01fbba
# ;madhu 250604 1.0.0-r1 v1.0.0-24-gb44a198

EAPI=8
PYTHON_COMPAT=( python3_{11..13} )

inherit xdg meson gnome.org python-single-r1 systemd gnome2-utils

DESCRIPTION="Change the frequency limits of the cpu and its governor"
HOMEPAGE="https://GitHub.com/vagnum08/cpupower-gui"

USE_GIT=true
if ${USE_GIT}; then
	inherit git-r3
	SRC_URI=""
	EGIT_REPO_URI="https://GitHub.com/vagnum08/cpupower-gui"
	EGIT_BRANCH="madhu"
fi

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="x86 amd64"

RDEPEND="${PYTHON_DEPS}"
DEPEND="${RDEPEND}
	$(python_gen_cond_dep '
		>=dev-python/pygobject-3.10.2:3[${PYTHON_USEDEP}]
		dev-python/pygobject:3[${PYTHON_USEDEP}]
	')
	>=dev-libs/glib-2.58:2
	>=x11-libs/gtk+-3.12:3[introspection]
"
BDEPEND=">=sys-devel/gettext-0.19.8"

src_prepare() {
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	sed -i -e "s|^meson.add_install_script|#meson.add_install_script|g" meson.build
	default
}
pkg_setup() {
	python-single-r1_pkg_setup
}

src_configure() {
	local emesonargs=(
		-Dsystemddir="$(systemd_get_systemunitdir)"
	)
	meson_src_configure
}

src_install() {
	meson_src_install
	sed -i -e 's:^#!.*python3:#!/usr/bin/env python3:' "${ED}"/usr/bin/* || die
	python_fix_shebang "${ED}"/usr/bin/

}
pkg_postinst() {
	xdg_pkg_postinst
	gnome2_schemas_update
}

pkg_postrm() {
	xdg_pkg_postrm
	gnome2_schemas_update
}

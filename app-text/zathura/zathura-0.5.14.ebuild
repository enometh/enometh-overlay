# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Aug 25 14:08:21 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200825 0.4.5 -> 0.4.6
# ;madhu 211004 0.4.8
# ;madhu 221113 0.4.9 (unreleased)
# ;madhu 230306 0.5.2-r4, -magic, build manpages
# ;madhu 250324 0.5.11 - disable landlock
# ;madhu 260419 0.5.14

EAPI=8

inherit flag-o-matic meson xdg

DESCRIPTION="A highly customizable and functional document viewer"
HOMEPAGE="https://pwmt.org/projects/zathura/"

if [[ ${PV} == *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/pwmt/zathura.git"
else
	SRC_URI="https://github.com/pwmt/zathura/archive/${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="~amd64 ~arm ~arm64 ~riscv ~x86"
fi

LICENSE="ZLIB"
#SLOT="0/6.7" # plugin versions api.abi (see meson.build)
SLOT="0/$(ver_cut 1-2)"
IUSE="+man seccomp synctex test wayland X"

RESTRICT="!test? ( test )"
REQUIRED_USE="
	test? ( X )
	|| ( wayland X )
"

# 	>=dev-libs/girara-0.4.5-r1:=[X?]
RDEPEND="
	dev-libs/json-glib
	man? ( dev-python/sphinx )
	dev-db/sqlite:3
	>=dev-libs/glib-2.76:2
	dev-libs/girara
	sys-apps/file
	x11-libs/cairo
	>=x11-libs/gtk+-3.24:3[wayland?,X?]
	x11-libs/pango
	man? ( dev-python/sphinx )
	seccomp? ( sys-libs/libseccomp )
	synctex? ( app-text/texlive-core )
"
DEPEND="
	${RDEPEND}
"
# 	>=sys-kernel/linux-headers-5.13

BDEPEND="
	>=sys-devel/gettext-0.19.8
	virtual/pkgconfig
	test? (
		dev-libs/appstream
		x11-misc/xvfb-run
	)
"

PATCHES=(
	${FILESDIR}/zathura-0.5.14-synctex-version-patch
)

src_prepare() {
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	default
}

src_configure() {
	local emesonargs=(
		-Dconvert-icon=disabled
		-Dlandlock=disabled
		$(meson_feature man manpages)
		$(meson_feature seccomp)
		$(meson_feature synctex)
		$(meson_feature test tests)
		)
	meson_src_configure
}

src_install() {
	meson_src_install

	if use seccomp ; then # || use landlock
		mv "${ED}"/usr/bin/zathura{,-full} || die
		dosym zathura-sandbox /usr/bin/zathura
	fi
}

pkg_postinst() {
	if use seccomp; then #  || use landlock
		elog "Zathura has been installed as a symlink to zathura-sandbox due to USE"
		elog "seccomp or USE landlock.  Some features such as printing or hyperlinks"
		elog "may be unavailable when running with the default executable (zathura)."
		elog "If you require these features, you can temporarily switch to using"
		elog "zathura-full or disable these use flags."
		if ! use elibc_glibc; then
			ewarn ""
			ewarn "Upstream zathura does not test sandboxing rules on non-glibc"
			ewarn "environments.  Your mileage may vary using the sandboxed variant."
		fi
	fi
}

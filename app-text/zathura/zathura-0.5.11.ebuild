# Copyright 1999-2025 Gentoo Authors
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

EAPI=8

inherit meson virtualx xdg

DESCRIPTION="A highly customizable and functional document viewer"
HOMEPAGE="https://pwmt.org/projects/zathura/"

if [[ ${PV} == *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://git.pwmt.org/pwmt/${PN}.git"
	EGIT_BRANCH="develop"
else
	SRC_URI="
		https://github.com/pwmt/zathura/archive/${PV}.tar.gz -> ${P}.tar.gz
	"
	KEYWORDS="~amd64 arm ~arm64 ~riscv x86 ~amd64-linux ~x86-linux"
fi

LICENSE="ZLIB"
#SLOT="0/6.7"
SLOT="0/$(ver_cut 1-2)"
IUSE="+man seccomp sqlite synctex test"

RESTRICT="!test? ( test )"

RDEPEND="
	dev-libs/json-glib
	man? ( dev-python/sphinx )
	seccomp? ( sys-libs/libseccomp )
	synctex? ( app-text/texlive-core )
	sys-apps/file
	x11-libs/cairo
	x11-libs/pango
	>=dev-db/sqlite-3.6.23:3
	>=dev-libs/girara-0.4.5:=
	>=dev-libs/glib-2.72:2
	>=x11-libs/gtk+-3.24:3
"
DEPEND="
	${RDEPEND}
	test? (
		dev-libs/check
		>=x11-libs/gtk+-3.24:3[X]
	)
"
# 	>=sys-kernel/linux-headers-5.13

BDEPEND="
	>=sys-devel/gettext-0.19.8
	virtual/pkgconfig
"

src_prepare() {
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	default
}

src_configure() {
	local emesonargs=(
		-Dconvert-icon=disabled
		-Dlandlock=disabled
		-Dmanpages=$(usex man enabled disabled)
		-Dseccomp=$(usex seccomp enabled disabled)
		-Dsynctex=$(usex synctex enabled disabled)
		)
	meson_src_configure
}

src_test() {
	virtx meson_src_test
}

src_install() {
	meson_src_install
}

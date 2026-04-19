# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Aug 25 13:58:00 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200825 - 0.3.4 -> 0.3.5
# ;madhu 211004 - 0.3.6
# ;madhu 230306 - 0.3.9
# ;madhu 230823 - 0.4.0, debug
# ;madhu 250324 - 0.4.5
# ;madhu 260419 - 0.4.5-r1
# ;madhu 260419 - 2026.02.04

EAPI=8

inherit flag-o-matic meson

DESCRIPTION="UI library that focuses on simplicity and minimalism"
HOMEPAGE="https://pwmt.org/projects/girara/"

if [[ ${PV} == *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/pwmt/${PN}.git"
	EGIT_BRANCH="develop"
else
	SRC_URI="https://github.com/pwmt/girara/archive/${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="~amd64 ~arm ~arm64 ~riscv ~x86"
fi

LICENSE="ZLIB"
SLOT="0/$(ver_cut 2-3)"
IUSE="doc libnotify test X"
RESTRICT="!test? ( test )"

RDEPEND="
	app-accessibility/at-spi2-core
	>=dev-libs/glib-2.72:2
	dev-libs/json-glib:=
	media-libs/harfbuzz:=
	x11-libs/cairo[glib]
	x11-libs/gdk-pixbuf
	>=x11-libs/gtk+-3.24:3[X?]
	x11-libs/pango
	libnotify? ( x11-libs/libnotify )
"
DEPEND="
	${RDEPEND}
	test? (
		x11-base/xorg-proto
		x11-libs/gtk+:3[X]
		x11-misc/xvfb-run
	)
"
BDEPEND="
	sys-devel/gettext
	virtual/pkgconfig
	doc? ( app-text/doxygen )
"

DOCS=( AUTHORS README.md )

src_prepare() {
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	default
}

src_configure() {
	# defang automagic dependencies
	# Currently only needed for X11-specific workarounds
	# use X || append-flags -DGENTOO_GTK_HIDE_X11

	local -a emesonargs=(
		$(meson_feature doc docs)
		$(meson_feature test tests)
		--debug
	)
	meson_src_configure
}

src_compile() {
	meson_src_compile
	use doc && HTML_DOCS=( "${BUILD_DIR}"/doc/html/. ) # BUILD_DIR is set by meson_src_compile
}

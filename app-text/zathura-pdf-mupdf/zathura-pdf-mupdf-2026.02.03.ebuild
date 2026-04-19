# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

#   Time-stamp: <>
#   Touched: Mon Oct 04 10:32:33 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 211004 0.3.6 -> 0.3.7
# ;madhu 221113 0.3.9
# ;madhu 230112 0.4.0
# ;madhu 250325 0.4.4 - fixed for mupdf
# ;madhu 260419 2026.02.03

EAPI=8

inherit meson xdg

if [[ ${PV} == *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/pwmt/zathura-pdf-mupdf.git"
	EGIT_BRANCH="develop"
else
	KEYWORDS="amd64 arm x86"
	SRC_URI="https://github.com/pwmt/${PN}/archive/${PV}.tar.gz -> ${P}.tar.gz"
fi

DESCRIPTION="PDF support for zathura using the mupdf PDF rendering library"
HOMEPAGE="https://pwmt.org/projects/zathura-pdf-mupdf/"

LICENSE="ZLIB"
SLOT="0"
IUSE="+javascript"

# Tests currently only validating data files
RESTRICT="test"

DEPEND="
	>=app-text/mupdf-1.24.0:=[javascript?]
	>=app-text/zathura-0.2.0:=
	dev-libs/girara:=
	dev-libs/glib:2
	x11-libs/cairo
"

RDEPEND="${DEPEND}"

BDEPEND="virtual/pkgconfig"

PATCHES=(
#	"${FILESDIR}/zathura-pdf-mupdf-0.4.3-meson-mupdfthird.patch"
)

src_prepare() (
	default

	if ! use javascript ; then
		sed -i -e '/mujs/d' meson.build || die
	fi
)

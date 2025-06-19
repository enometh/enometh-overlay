# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#
#   Time-stamp: <>
#   Touched: Tue Mar 28 20:29:27 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230328 - 2.3.3
# ;madhu 250619 - 2.3.4
EAPI=8

inherit toolchain-funcs xdg-utils

DESCRIPTION="X based system monitor"
HOMEPAGE="https://sourceforge.net/projects/xosview/"

# TODO USE_GIT clone https://git.code.sf.net/p/xosview/git xosview-git

# ;madhu 230412 hasn't sf mirror with portage wget been busted for years?
# you have to download files manually and put them in distdir.
#SRC_URI=mirror://sourceforge/${PN}/${P}.tar.xz
SRC_URI="http://master.dl.sourceforge.net/project/xosview/${P}.tar.gz?viasf=1 -> ${P}.tar.gz"

# "There is a balance to leaving debug messages in the code and filling
# your fellow developer's screen with the a stream of information
# resembling the matrix on crack"
#		-- Mike Romberg (xosview2/doc/README.devel)
#
IUSE="debug truetype"

LICENSE="GPL-2 BSD"
SLOT="0"
KEYWORDS="amd64 ppc ppc64 x86"

COMMON_DEPS="x11-libs/libX11
	x11-libs/libXpm
	truetype? ( x11-libs/libXft >=media-libs/freetype-2 )
"
RDEPEND="${COMMON_DEPS}
	media-fonts/font-misc-misc"
DEPEND="${COMMON_DEPS}
	x11-base/xorg-proto"

PATCHES=(
	$FILESDIR/xosview2-2.3.3-linux-btrymeter.cc-BtryMeter-getcapacity-accomodate-.patch
	$FILESDIR/xosview2-2.3.3-linux-memmeter.cc-MemMeter-getmeminfo-do-not-subtrac.patch
	$FILESDIR/xosview2-2.3.3-log.cc-Log-readConfig-use-environment-variable-LOGCO.patch
	$FILESDIR/xosview2-2.3.3-xftfont.c-X11ftFont-setFont-log-missing-font.patch
)

DOCS+=(doc)

src_configure() {
	local myeconfargs=(
		$(use_enable debug)
		$(use_with truetype freetype2)
	)
	econf "${myeconfargs[@]}"
}

# if USE=debug then use LOGCONF="<(echo -e '- *.cc\n- *.h)"
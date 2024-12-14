# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Jun 06 22:02:10 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230606 1.8.5 -- xorg.eclass should not remove manpages during
#   install just because `doc' is not set in IUSE. remember to fix the
#   eclass. svg docs should not be compresed. don't use
#   xorg-3_src_configure as docs get built for each arch during multilib
#   compile.
# ;madhu 241214 1.18.10

EAPI=8

XORG_DOC=doc
XORG_MULTILIB=yes
XORG_TARBALL_SUFFIX=xz
inherit toolchain-funcs xorg-3

# Note: please bump this with x11-misc/compose-tables
DESCRIPTION="X.Org X11 library"

KEYWORDS="~alpha amd64 arm arm64 hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos"
IUSE="test"
RESTRICT="!test? ( test )"

RDEPEND="
	>=x11-libs/libxcb-1.11.1[${MULTILIB_USEDEP}]
	x11-misc/compose-tables
"
DEPEND="${RDEPEND}
	x11-base/xorg-proto
	x11-libs/xtrans
"
BDEPEND="test? ( dev-lang/perl )"

src_configure() {
	local XORG_CONFIGURE_OPTIONS=(
		$(use_with doc xmlto)
		$(use_enable doc specs)
		--enable-ipv6
		--without-fop
		--with-keysymdefdir="${ESYSROOT}/usr/include/X11"
		CPP="$(tc-getPROG CPP cpp)"
	)
	xorg-3_src_configure
}

src_install() {
	xorg-3_src_install
	for i in XIM XKB i18n libX11; do
		find ${ED}/usr/share/doc/${PF}/$i \( -name \*.xml -o -name \*.db \) -exec rm -fv \{\} \;
		docompress -x /usr/share/doc/${PF}/$i
		# FIXME: 1 .txt file is left uncompressed.
	done
	rm -rf "${ED}"/usr/share/X11/locale || die
}

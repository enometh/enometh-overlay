# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed May 20 12:08:07 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260520 1.8.0

EAPI=8
inherit toolchain-funcs out-of-source libtool

DESCRIPTION="X.509 and CMS (PKCS#7) library"
HOMEPAGE="https://www.gnupg.org/related_software/libksba"
SRC_URI="mirror://gnupg/${PN}/${P}.tar.bz2"

LICENSE="LGPL-3+ GPL-2+ GPL-3"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 ~sparc x86 ~arm64-macos ~x64-macos ~x64-solaris"
IUSE="static-libs"

RDEPEND=">=dev-libs/libgpg-error-1.33"
DEPEND="${RDEPEND}"
BDEPEND="
	app-alternatives/yacc
"

PATCHES=(
#	"${FILESDIR}"/${PN}-1.6.0-no-fgrep-ksba-config.patch
)

src_prepare() {
	default

	elibtoolize  # necessary on Solaris for shared lib support
}

my_src_configure() {
	export CC_FOR_BUILD="$(tc-getBUILD_CC)"

	local myeconfargs=(
		--disable-valgrind-tests
		$(use_enable static-libs static)

		GPG_ERROR_CONFIG="${ESYSROOT}/usr/bin/${CHOST}-gpg-error-config"
		LIBGCRYPT_CONFIG="${ESYSROOT}/usr/bin/${CHOST}-libgcrypt-config"
		GPGRT_CONFIG="${ESYSROOT}/usr/bin/${CHOST}-gpgrt-config"
	)

	econf "${myeconfargs[@]}"
}

my_src_install() {
	default

	# People need to use ksba-config for --cflags and --libs
	find "${ED}" -type f -name '*.la' -delete || die
}

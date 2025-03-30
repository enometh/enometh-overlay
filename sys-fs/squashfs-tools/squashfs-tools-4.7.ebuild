# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Mar 28 09:32:36 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 190325  9999 3.2-1659-g6e242dc9
# ;madhu 250329  4.7 (unreleased) 4.6-836-gc0ffe4cb

EAPI=8
USE_GIT=true

inherit flag-o-matic toolchain-funcs

DESCRIPTION="Tools to create and extract Squashfs filesystems"
HOMEPAGE="
	http://squashfs.sourceforge.net
	https://github.com/plougher/squashfs-tools
"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/plougher/${PN}"
	EGIT_CLONE_TYPE=shallow
	EGIT_BRANCH=master
else
SRC_URI="
	https://github.com/plougher/squashfs-tools/archive/${PV}.tar.gz
		-> ${P}.tar.gz
"
fi

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86"
IUSE="debug lz4 lzma lzo static xattr +xz zstd"

DEPEND="
	sys-libs/zlib
	lz4? ( app-arch/lz4 )
	lzma? ( app-arch/xz-utils )
	lzo? ( dev-libs/lzo )
	xattr? ( sys-apps/attr )
	zstd? ( app-arch/zstd )
"
RDEPEND=${DEPEND}

PATCHES=(
#	"${FILESDIR}"/${PN}-4.3-sysmacros.patch
#	"${FILESDIR}"/${PN}-4.3-aligned-data.patch
)

use10() {
	usex "${1}" 1 0
}

src_configure() {
	if ${USE_GIT}; then
	cd "${WORKDIR}"/${P}/${PN} || die
	fi

	# set up make command line variables in EMAKE_SQUASHFS_CONF
	EMAKE_SQUASHFS_CONF=(
		LZ4_SUPPORT=$(use10 lz4)
		LZMA_XZ_SUPPORT=$(use10 lzma)
		LZO_SUPPORT=$(use10 lzo)
		XATTR_SUPPORT=$(use10 xattr)
		XZ_SUPPORT=$(use10 xz)
		ZSTD_SUPPORT=$(use10 zstd)
	)

	tc-export CC
	use debug && append-cppflags -DSQUASHFS_TRACE
	use static && append-ldflags -static
}

src_compile() {
	if ${USE_GIT}; then
	cd "${WORKDIR}"/${P}/${PN} || die
	fi
	emake "${EMAKE_SQUASHFS_CONF[@]}"
}

src_install() {
	dobin "${WORKDIR}"/${P}/${PN}/{mksquashfs,unsquashfs}
	dodoc CHANGES README* Documentation/4.7/README
	doman Documentation/latest/manpages/*.1

	dosym unsquashfs /usr/bin/sqfscat
	dosym mksquashfs /usr/bin/sqfstar
}

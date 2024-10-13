# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Nov 16 15:07:54 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 201116 0.90 - build master from git, localpatch qmerge pkg_merge
# ;madhu 210317 0.91 v0.91-6-gec6803a
# ;madhu 210627 0.91-r1 v0.91-43-gfd68fb7 git master, localpatch
# ;madhu 210910 0.92 v0.92-2-g4d933a8
# ;madhu 220403 0.93.3 v0.93.3-48-gb65e7f0
# ;madhu 220817 0.94.1 v0.94-1-g62503bb
# ;madhu 231012 0.96.1 v0.96.1
# ;madhu 241013 0.97 v0.97-9-g3a525dc (patches to ship a libq.so to facilitate use from lisp via ffi)

EAPI=8
USE_GIT=true

inherit flag-o-matic toolchain-funcs

DESCRIPTION="Small and fast Portage helper tools written in C"
HOMEPAGE="https://wiki.gentoo.org/wiki/Portage-utils"

if [[ ${PV} == *9999 ]] || ${USE_GIT} ; then
	inherit git-r3 autotools
	EGIT_REPO_URI="https://anongit.gentoo.org/git/proj/portage-utils.git"
else
	SRC_URI="https://dev.gentoo.org/~grobian/distfiles/${P}.tar.xz"
fi

	KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"

LICENSE="GPL-2"
SLOT="0"
IUSE="openmp +qmanifest +qtegrity static"

RDEPEND="
	openmp? ( || (
		sys-devel/gcc:*[openmp]
		sys-libs/libomp
	) )
	qmanifest? (
		!static? (
			app-crypt/gpgme:=
			app-crypt/libb2:=
			dev-libs/openssl:=
			sys-libs/zlib:=
		)
	)
	qtegrity? (
		!static? (
			dev-libs/openssl:=
		)
	)"
DEPEND="${RDEPEND}
	qmanifest? (
		static? (
			app-crypt/gpgme[static-libs]
			app-crypt/libb2[static-libs]
			dev-libs/openssl[static-libs]
			sys-libs/zlib[static-libs]
		)
	)
	qtegrity? (
		static? (
			dev-libs/openssl[static-libs]
		)
	)"
BDEPEND="virtual/pkgconfig"

# bug #898362, gnulib check explicitly looks for MIN in some headers
QA_CONFIG_IMPL_DECL_SKIP="MIN"

PATCHES=(
	${FILESDIR}/portage-utils-47.0-configure.ac-reintroduce-libtool.patch
	${FILESDIR}/portage-utils-47.0-libq-Makefile.am-install-libq.a-and-libq.so-but-link.patch
)

pkg_setup() {
	[[ ${MERGE_TYPE} != binary ]] && use openmp && tc-check-openmp
}

src_prepare() {
	default
	[[ ${PV} == *9999 ]] && eautoreconf
}

src_configure() {
	use static && append-ldflags -static

	econf \
		--disable-maintainer-mode \
		--with-eprefix="${EPREFIX}" \
		$(use_enable qmanifest) \
		$(use_enable qtegrity) \
		$(use_enable openmp)
}

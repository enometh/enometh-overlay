# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed May 20 14:00:31 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260520 2.1.0, remove gentoo cruft: verify-sig

EAPI=8

inherit cmake

DESCRIPTION="GnuPG Made Easy is a library for making GnuPG easier to use (C++ bindings)"
HOMEPAGE="https://www.gnupg.org/related_software/gpgme"
SRC_URI="
	mirror://gnupg/${PN}/${P}.tar.xz
"

LICENSE="LGPL-2+ test? ( GPL-2 GPL-2+ LGPL-2.1+ )"
SLOT="0/7"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 ~sparc x86 ~arm64-macos ~x64-macos ~x64-solaris"
IUSE="test"
RESTRICT="!test? ( test )"

DEPEND="
	>=app-crypt/gpgme-${PV%.*}:=
	>=dev-libs/libgpg-error-1.47:=
"
RDEPEND="${DEPEND}
	!<app-crypt/gpgme-2[cxx(-)]
	!dev-cpp/gpgmepp:1
"

src_configure() {
	local mycmakeargs=(
		# As of 2.0.0, there aren't any non-manual tests. tests/README
		# says that the real testing is done via dev-libs/qgpgme instead.
		-DBUILD_TESTING=$(usex test)
	)

	cmake_src_configure
}

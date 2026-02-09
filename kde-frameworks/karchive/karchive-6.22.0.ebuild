# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 11:47:01 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 (alt Qt6)

EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for reading, creation, and manipulation of various archive formats"

LICENSE="GPL-2 LGPL-2.1"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="crypt +zstd"

DEPEND="
	app-arch/bzip2
	app-arch/xz-utils
	virtual/zlib:=
	crypt? ( dev-libs/openssl:= )
	zstd? ( app-arch/zstd:= )
"
RDEPEND="${DEPEND}"
BDEPEND="
	dev-qt/qttools[linguist]
	zstd? ( virtual/pkgconfig )
"

src_prepare() {
	ecm_src_prepare

	find po -type f -exec rm -fv '{}' ';'

	# TODO: try to get a build switch upstreamed
	if ! use zstd; then
		sed -e "s/^pkg_check_modules.*LibZstd/#&/" -i CMakeLists.txt || die
	fi
}

src_configure() {
	local mycmakeargs=(
		-DWITH_OPENSSL=$(usex crypt)
		-DWITH_LIBZSTD=$(usex zstd)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

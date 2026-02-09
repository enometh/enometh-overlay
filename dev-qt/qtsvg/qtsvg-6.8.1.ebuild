# Copyright 2021-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 16:00:04 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.8.1 alt Qt (demo)

EAPI=8
QTVER=6.8.1

inherit qt6-build toolchain-funcs

DESCRIPTION="SVG rendering library for the Qt6 framework"

if [[ ${QT6_BUILD_TYPE} == release ]]; then
	KEYWORDS="amd64 arm arm64 ~hppa ~loong ppc ppc64 ~riscv x86"
fi

RDEPEND="
	dev-qt/qtbase:6[gui,widgets]
	virtual/zlib:=
"
DEPEND="${RDEPEND}"

src_test() {
	# tst_QSvgRenderer::testFeColorMatrix (new in 6.7, likely low impact)
	# is known failing on BE, could use more looking into (bug #935356)
	[[ $(tc-endian) == big ]] && local CMAKE_SKIP_TESTS=( tst_qsvgrenderer )

	qt6-build_src_test
}

src_configure() {
	mycmakeargs=(
		-DCMAKE_INSTALL_PREFIX=/opt/Qt${QTVER}
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	qt6-build_src_configure
}

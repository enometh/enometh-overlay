# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 08:36:16 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 -- ignore gentoo qt

EAPI=8

ECM_EXAMPLES="true"
ECM_QTHELP="false"
ECM_TEST="true"
QTVER=6.8.1
inherit ecm frameworks.kde.org toolchain-funcs

DESCRIPTION="Lightweight user interface framework for mobile and convergent applications"
HOMEPAGE="https://community.kde.org/Kirigami"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="openmp"

# requires package to already be installed
RESTRICT="test"

# slot op: Uses Qt6::GuiPrivate for qguiapplication_p.h
DEPEND="
	dev-qt/qtbase[concurrent,dbus,gui,network]
	dev-qt/qtdeclarative
	dev-qt/qtsvg
"
RDEPEND="${DEPEND}
	examples? (
		dev-qt/qt5compat[qml]
	)
"
BDEPEND="dev-qt/qttools[linguist]"

pkg_pretend() {
	[[ ${MERGE_TYPE} != binary ]] && use openmp && tc-check-openmp
}

pkg_setup() {
	[[ ${MERGE_TYPE} != binary ]] && use openmp && tc-check-openmp
}

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		-DBUILD_EXAMPLES=$(usex examples)
		$(cmake_use_find_package openmp OpenMP)
	)

	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

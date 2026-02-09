# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 15:50:55 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt

EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework to install and load packages of non binary content"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="man"

DEPEND="
	dev-qt/qtbase:6[dbus]
	kde-frameworks/karchive:6
	kde-frameworks/kcoreaddons:6
	kde-frameworks/ki18n:6
"
RDEPEND="${DEPEND}"
BDEPEND="man? ( kde-frameworks/kdoctools:6 )"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		$(cmake_use_find_package man KF6DocTools)
	)

	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

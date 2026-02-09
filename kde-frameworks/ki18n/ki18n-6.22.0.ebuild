# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 09:50:28 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 (alt qt)

EAPI=8

PYTHON_COMPAT=( python3_{11..14} )
QTVER=6.8.1
inherit ecm frameworks.kde.org python-single-r1

DESCRIPTION="Framework based on Gettext for internationalizing user interface text"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

REQUIRED_USE="${PYTHON_REQUIRED_USE}"

COMMON_DEPEND="${PYTHON_DEPS}
	dev-qt/qtbase[widgets]
	dev-qt/qtdeclarative
	sys-devel/gettext
	virtual/libintl
"
DEPEND="${COMMON_DEPEND}
	test? ( dev-qt/qtbase[concurrent] )
"
RDEPEND="${COMMON_DEPEND}
	app-text/iso-codes
"
src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		-DPython3_EXECUTABLE="${PYTHON}"
	)
#	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

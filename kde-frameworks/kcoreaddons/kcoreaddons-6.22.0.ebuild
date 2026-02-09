# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 09:36:01 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 (alt qt)

EAPI=8

ECM_PYTHON_BINDINGS="off"
QTVER=6.8.1
inherit ecm frameworks.kde.org xdg

DESCRIPTION="Framework for solving common problems such as caching, randomisation, and more"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="dbus"

COMMON_DEPEND="
	dev-qt/qtbase[dbus?,icu,network]
	dev-qt/qtdeclarative
	virtual/libudev:=
"
DEPEND="${COMMON_DEPEND}
	sys-kernel/linux-headers
"
RDEPEND="${COMMON_DEPEND}
	dev-qt/qttranslations
"
BDEPEND="dev-qt/qttools[linguist]"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		-DKCOREADDONS_USE_QML=ON
		-DENABLE_INOTIFY=ON
		-DUSE_DBUS=$(usex dbus)
	)
	ecm_src_configure
}

# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 15:46:36 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt

EAPI=8

ECM_PYTHON_BINDINGS="off"
ECM_TEST="false"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for notifying the user of an event"

LICENSE="LGPL-2.1+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"

#	dev-qt/qtdeclarative:6
RDEPEND="
	dev-qt/qtbase:6[dbus,gui]
	kde-frameworks/kconfig:6
	media-libs/libcanberra
"
DEPEND="${RDEPEND}"
BDEPEND="dev-qt/qttools:6[linguist]"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 09:21:00 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 - alt Qt

EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for reading and writing configuration"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="dbus qml"
REQUIRED_USE="test? ( qml )"

# bug 560086
RESTRICT="test"

RDEPEND="
	dev-qt/qtbase[dbus?,gui,xml]
	qml? ( dev-qt/qtdeclarative6 )
"
DEPEND="${RDEPEND}
	test? ( dev-qt/qtbase[concurrent] )
"
BDEPEND="dev-qt/qttool[linguist]"

DOCS=( DESIGN docs/{DESIGN.kconfig,options.md} )

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		-DUSE_DBUS=$(usex dbus)
		-DKCONFIG_USE_QML=$(usex qml)
	)
#	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

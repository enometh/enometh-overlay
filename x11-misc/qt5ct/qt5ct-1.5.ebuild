# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Sep 10 09:36:54 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
#;madhu 210910 1.3 - no cmake
#;madhu 220715 1.5

EAPI=8

inherit qmake-utils

DESCRIPTION="Qt5 configuration tool, similar to qtconfig for Qt4"
HOMEPAGE="https://sourceforge.net/projects/qt5ct/"
SRC_URI="https://download.sourceforge.net/qt5ct/${P}.tar.bz2"

LICENSE="BSD-2"
SLOT="0"
KEYWORDS="~amd64 ~arm ~ppc64 ~x86"
IUSE="+dbus"

RDEPEND="
	dev-qt/qtconcurrent:5
	dev-qt/qtcore:5
	dev-qt/qtgui:5=
	dev-qt/qtwidgets:5
	dbus? (
		dev-qt/qtdbus:5
		dev-qt/qtgui:5[dbus]
	)
"
DEPEND="${RDEPEND}"
BDEPEND="
	dev-qt/linguist-tools:5
	dev-qt/qtpaths:5
"

src_configure() {
	eqmake5 DISABLE_DBUS=$(usex !dbus 1 0)
}

src_install() {
	emake INSTALL_ROOT="${D}" install
	einstalldocs

	newenvd - 98${PN} <<< 'QT_QPA_PLATFORMTHEME=qt5ct'
}

pkg_postinst() {
	if [[ -z ${REPLACING_VERSIONS} ]]; then
		ewarn "qt5ct configuration won't be applied to the currently running sessions."
		ewarn "Please relogin."
	fi
	if ! has_version 'dev-qt/qtsvg:5'; then
		elog
		elog "For SVG icon themes, please install 'dev-qt/qtsvg:5'."
		elog
	fi
}

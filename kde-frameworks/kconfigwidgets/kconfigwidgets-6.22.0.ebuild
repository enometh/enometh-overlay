# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 12:04:15 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt Qt

EAPI=8

ECM_DESIGNERPLUGIN="true"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework providing an assortment of configuration-related widgets"

LICENSE="LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

RDEPEND="
	dev-qt/qtbase[dbus,gui,widgets]
	kde-frameworks/kcodecs
	kde-frameworks/kcolorscheme
	kde-frameworks/kconfig
	kde-frameworks/kcoreaddons
	kde-frameworks/kguiaddons
	kde-frameworks/ki18n
	kde-frameworks/kwidgetsaddons
"
DEPEND="${RDEPEND}
	test? ( kde-frameworks/kconfig[dbus] )
"

src_prepare() {
	ecm_src_prepare
	find po -type f -exec rm -fv '{}' ';'
	rm -rfv src/language_files/*
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

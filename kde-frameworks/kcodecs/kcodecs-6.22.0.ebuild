# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 12:32:54 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 alt qt

EAPI=8

QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for manipulating strings using various encodings"

LICENSE="GPL-2+ LGPL-2+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE=""

BDEPEND="dev-qt/qttools[linguist]"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

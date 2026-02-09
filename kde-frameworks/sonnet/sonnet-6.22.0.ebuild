# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 10:01:33 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 (alt Qt)

EAPI=8

ECM_DESIGNERPLUGIN="true"
QTVER=6.8.1
inherit ecm frameworks.kde.org

DESCRIPTION="Framework for providing spell-checking through abstraction of popular backends"

LICENSE="LGPL-2+ LGPL-2.1+"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="aspell +hunspell qml"

DEPEND="
	dev-qt/qtbase[gui,widgets]
	aspell? ( app-text/aspell )
	hunspell? ( app-text/hunspell:= )
	qml? ( dev-qt/qtdeclarative )
"
RDEPEND="${DEPEND}"
BDEPEND="dev-qt/qttools[linguist]"

src_prepare() {
	ecm_src_prepare
	find poqm -type f -exec rm -fv '{}' ';'
}

src_configure() {
	local mycmakeargs=(
		$(cmake_use_find_package aspell ASPELL)
		$(cmake_use_find_package hunspell HUNSPELL)
		-DSONNET_USE_QML=$(usex qml)
	)
	if ! use aspell && ! use hunspell; then
		mycmakeargs+=( -DSONNET_NO_BACKENDS=ON )
	fi

	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

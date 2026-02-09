# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 14:34:47 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.22.0 Alt qt

EAPI=8
QTVER=6.8.1

ECM_QTHELP="false"
inherit ecm frameworks.kde.org

DESCRIPTION="Tools to generate documentation in various formats from DocBook files"

LICENSE="MIT"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="nls"

DEPEND="
	app-text/docbook-xml-dtd:4.5
	app-text/docbook-xsl-stylesheets
	app-text/sgml-common
	dev-libs/libxml2:2=
	dev-libs/libxslt
	kde-frameworks/karchive:6
"
RDEPEND="${DEPEND}"
BDEPEND="
	dev-lang/perl
	dev-perl/URI
	nls? ( kde-frameworks/ki18n:6 )
"

PATCHES=( "${FILESDIR}/${PN}-5.54.0-gentoo-docbundledir.patch" )

src_configure() {
	local mycmakeargs=(
		$(cmake_use_find_package nls KF6I18n)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

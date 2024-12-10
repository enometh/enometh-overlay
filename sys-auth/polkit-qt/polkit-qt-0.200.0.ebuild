# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Dec 05 15:35:47 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241205 - 0.200.0 - provide "polkit-qt-6-1",  try to liberate from gentoo qt6, kde packaging

EAPI=8

QT6ROOT=/opt/Qt6.8.1

#KDE_ORG_CATEGORY="libraries"
KDE_ORG_NAME="polkit-qt-1"
inherit cmake # multibuild

DESCRIPTION="Qt wrapper around polkit-1 client libraries"
HOMEPAGE="https://api.kde.org/polkit-qt-1/html/"

#if [[ ${KDE_BUILD_TYPE} = release ]]; then
SRC_URI="mirror://kde/stable/${KDE_ORG_NAME}/${KDE_ORG_NAME}-${PV}.tar.xz"
KEYWORDS="amd64 ~arm arm64 ~loong ~ppc ppc64 ~riscv x86"
#fi

LICENSE="LGPL-2"
SLOT="0"
# IUSE="+qt5 qt6"
# REQUIRED_USE="|| ( qt5 qt6 )"

RDEPEND="
	dev-libs/glib:2
	>=sys-auth/polkit-0.103
"

# "
# 	qt5? (
# 		dev-qt/qtcore:5
# 		dev-qt/qtdbus:5
# 		dev-qt/qtgui:5
# 		dev-qt/qtwidgets:5
# 	)
# 	qt6? ( dev-qt/qtbase:6[dbus,gui,widgets] )
# "

DEPEND="${RDEPEND}"
BDEPEND="virtual/pkgconfig"

DOCS=( AUTHORS README README.porting TODO )

#pkg_setup() {
#	MULTIBUILD_VARIANTS=( $(usev qt5) $(usev qt6) )
#}

S="${WORKDIR}/${KDE_ORG_NAME}-${PV}"

src_configure() {
	myconfigure() {
		local mycmakeargs=(
			-DBUILD_EXAMPLES=OFF
			-DQT_MAJOR_VERSION=6
		)
		cmake_src_configure
	}
	CMAKE_PREFIX_PATH=${QT6ROOT}/lib/cmake \
					 PKG_CONFIG_PATH=${QT6ROOT}/lib/pkgconfig:$PKG_CONFIG_PATH \
					 myconfigure
#	multibuild_foreach_variant myconfigure
}

# src_compile() {
# 	multibuild_foreach_variant cmake_src_compile
# }

# src_install() {
# 	multibuild_foreach_variant cmake_src_install
# }

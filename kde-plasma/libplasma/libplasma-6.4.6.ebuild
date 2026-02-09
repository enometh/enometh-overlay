# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 14:05:03 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.4.6 alt qt

EAPI=8

ECM_QTHELP="true"
ECM_TEST="true"
KFMIN=6.18.0
QTVER=6.8.1
inherit ecm plasma.kde.org

DESCRIPTION="Plasma library and runtime components based upon KF6 and Qt6"

LICENSE="LGPL-2+"
SLOT="6"
KEYWORDS="~amd64 ~arm64 ~loong ~ppc64 ~riscv ~x86"
IUSE="gles2-only"

RESTRICT="test"

# dev-qt/qtbase slot op: includes qpa/qplatformwindow_p.h, qpa/qplatformwindow.h
# kde-frameworks/kwindowsystem[X]: Unconditional use of KX11Extras
# 	media-libs/libglvnd
# 	!gles2-only? ( media-libs/libglvnd[X] )
COMMON_DEPEND="
	dev-qt/qtbase:6=[dbus,gles2-only=,gui,opengl,widgets,X]
	dev-qt/qtdeclarative:6
	dev-qt/qtsvg:6
	dev-libs/wayland
	kde-frameworks/karchive:6
	kde-frameworks/kcolorscheme:6
	kde-frameworks/kconfig:6[qml]
	kde-frameworks/kcoreaddons:6
	kde-frameworks/kglobalaccel:6
	kde-frameworks/kguiaddons:6
	kde-frameworks/ki18n:6
	kde-frameworks/kiconthemes:6
	kde-frameworks/kio:6
	kde-frameworks/kirigami:6
	kde-frameworks/knotifications:6
	kde-frameworks/kpackage:6
	kde-frameworks/ksvg:6
	kde-frameworks/kwidgetsaddons:6
	kde-frameworks/kwindowsystem:6[X]
	kde-plasma/plasma-activities:6
	x11-libs/libX11
	x11-libs/libxcb
"
DEPEND="${COMMON_DEPEND}
	>=dev-libs/plasma-wayland-protocols-1.19.0
	x11-base/xorg-proto
"
RDEPEND="${COMMON_DEPEND}
	!${CATEGORY}/${PN}:5[-kf6compat(-)]
"
BDEPEND="
	dev-qt/qtbase:6[wayland]
	>=dev-util/wayland-scanner-1.19.0
"
src_configure() {
	local mycmakeargs=(
		$(cmake_use_find_package !gles2-only OpenGL)
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

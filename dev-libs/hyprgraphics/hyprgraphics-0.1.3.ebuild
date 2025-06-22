# Copyright 2023-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Jan 06 14:34:06 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250106 0.1.1
# ;madhu 250622 0.1.3

EAPI=8

inherit cmake

DESCRIPTION="Hyprland graphics / resource utilities"
HOMEPAGE="https://github.com/hyprwm/hyprgraphics"
SRC_URI="https://github.com/hyprwm/${PN}/archive/v${PV}.tar.gz -> ${P}.gh.tar.gz"

LICENSE="BSD"
SLOT="0"
KEYWORDS="~amd64"

RDEPEND="
	>=gui-libs/hyprutils-0.1.1:=
	media-libs/libjpeg-turbo:=
	media-libs/libjxl:=
	media-libs/libspng
	media-libs/libwebp:=
	sys-apps/file
	x11-libs/cairo
"
DEPEND="${RDEPEND}"

# PATCHES=( ${FILESDIR}/hyprgraphics-0.1.1-use-fmt-instead-of-c26.patch )
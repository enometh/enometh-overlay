# Copyright 2023-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Nov 23 14:43:43 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250106 0.6.0
# ;madhu 251123 0.6.6
# ;madhu 260108 0.6.8

EAPI=8

inherit cmake

DESCRIPTION="Official implementation library for the hypr config language"
HOMEPAGE="https://github.com/hyprwm/hyprlang"
SRC_URI="https://github.com/hyprwm/${PN}/archive/v${PV}.tar.gz -> ${P}.gh.tar.gz"

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64"

RDEPEND=">=gui-libs/hyprutils-0.1.1:="
DEPEND="${RDEPEND}"

# PATCHES=( ${FILESDIR}/hyprlang-0.6.0-use-fmt-instead-of-c26.patch )
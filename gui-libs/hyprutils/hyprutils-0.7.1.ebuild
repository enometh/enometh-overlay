# Copyright 2023-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#   Time-stamp: <>
#   Touched: Fri Nov 15 16:38:23 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241115 0.2.3 -> 0.2.5
# ;madhu 250106 0.3.9-r1 -> 0.3.2
# ;madhu 250205 0.3.2 -> 0.5.0
# ;madhu 250408 0.5.0 -> 0.6.0
# ;madhu 250622 0.6.0 -> 0.7.1

EAPI=8

inherit cmake

DESCRIPTION="Hyprland utilities library used across the ecosystem"
HOMEPAGE="https://github.com/hyprwm/hyprutils"

if [[ "${PV}" = *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/hyprwm/${PN^}.git"
else
	SRC_URI="https://github.com/hyprwm/${PN^}/archive/refs/tags/v${PV}/v${PV}.tar.gz -> ${P}.gh.tar.gz"
	S="${WORKDIR}/${PN}-${PV}"

	KEYWORDS="~amd64"
fi

LICENSE="BSD"
# SLOT="0/$(ver_cut 1-2)"
SLOT="0"

DEPEND="
	x11-libs/pixman
"
RDEPEND="${DEPEND}"

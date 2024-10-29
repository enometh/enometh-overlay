# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Oct 29 16:13:36 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241029 0.2.0  (0.2.0-16-g211fa63)

EAPI=8
USE_GIT=true

PYTHON_COMPAT=( python3_{10..13} )

inherit meson python-any-r1

DESCRIPTION="EDID and DisplayID library"
HOMEPAGE="https://gitlab.freedesktop.org/emersion/libdisplay-info"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://gitlab.freedesktop.org/emersion/libdisplay-info.git"
	EGIT_SUBMODULES=()
	EGIT_CLONE_TYPE="shallow"
	SRC_URI=""
else
	SRC_URI="https://gitlab.freedesktop.org/emersion/${PN}/-/releases/${PV}/downloads/${P}.tar.xz"
fi

LICENSE="MIT"
SLOT="0/2"
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~ppc64 ~riscv ~x86"

RDEPEND="sys-apps/hwdata"
DEPEND="${RDEPEND}"

BDEPEND="
	${PYTHON_DEPS}
	virtual/pkgconfig
"

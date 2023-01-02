# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Jan 02 11:08:11 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 230102 1.9.5 from 1.9.3-r1::dlang assume >=dmd-2.087 and
# >=gtkd-2.8.5[vte] are already installed, with pkgconfig files Libs:
# -L-L/usr/lib/dmd/2.087/lib64/ -L-lgtkd-3 -L-lvted-3 Cflags:
# -I/usr/include/dlang/gtkd-3/, requires a debug build

EAPI=8

DESCRIPTION="A tiling terminal emulator for Linux using GTK+ 3"
HOMEPAGE="https://gnunn1.github.io/tilix-web/"
LICENSE="MPL-2.0"

SLOT="0"
KEYWORDS="amd64 x86"
IUSE="+crypt"

DLANG_VERSION_RANGE="2.075-2.087"
DLANG_PACKAGE_TYPE="single"

# inherit dlang
inherit meson xdg

EMESON_BUILDTYPE=debug 			# XXX dmd bug.

GITHUB_URI="https://codeload.github.com/gnunn1"
SRC_URI="${GITHUB_URI}/${PN}/tar.gz/${PV} -> ${PN}-${PV}.tar.gz"

RDEPEND="
	>=sys-devel/gettext-0.19.8.1
	>=dev-libs/gtkd-3.8.5:3[vte]
	x11-libs/vte:2.91[crypt?]"
DEPEND="${RDEPEND}"
PATCHES=${FILESDIR}/tilix-1.9.5-no-i18n.patch

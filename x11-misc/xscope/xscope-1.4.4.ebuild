# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
# $Header: $
#
#   Time-stamp: <>
#   Touched: [Tue Oct 13 15:23:51 2020 +0530] <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 201013 - 1.4 (from rgv-overlay x11-misc)
# ;madhu 230731 - 1.4.4 x11-apps

EAPI=8

XORG_TARBALL_SUFFIX="xz"
XORG_MODULE="app/"
XORG_EAUTORECONF="yes"

inherit xorg-3

DESCRIPTION="a program to monitor X11/Client conversations"
HOMEPAGE="http://cgit.freedesktop.org/xorg/app/xscope/"
#SRC_URI="https://www.x.org/archive/individual/app/${P}.tar.bz2"
RESTRICT="mirror"

SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""

RDEPEND="
	x11-libs/libX11"
DEPEND="${RDEPEND}
	x11-base/xorg-proto"

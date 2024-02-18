# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Feb 18 19:51:25 2024 +0530
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# # ;madhu 240218 - pkgconfig-2; keep f.d.o pkg-config (pkgconf replacement is untrustworthy)

EAPI=7

DESCRIPTION="Virtual for the pkg-config implementation"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"

RDEPEND="
	|| (
		>=dev-util/pkgconf-1.3.7[pkg-config(+)]
		>=dev-util/pkgconfig-0.29.2
	)"

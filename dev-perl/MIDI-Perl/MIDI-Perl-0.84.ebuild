# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Mar 24 12:32:59 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
#
# ;madhu 250324 0.84

EAPI=8

DIST_AUTHOR=CONKLIN
DIST_VERSION=0.84
inherit perl-module

DESCRIPTION="Perl modules that allows you to read, compose, modify, and write MIDI files"

SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~x64-solaris"

RDEPEND="
"
BDEPEND="${RDEPEND}
dev-perl/Module-Build
"

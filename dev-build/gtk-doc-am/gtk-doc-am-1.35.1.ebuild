# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Dec 24 22:29:58 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 191224 1.25-r1 -> 1.32, buildsystems/autotools/gtk-doc.m4
# ;madhu 201112 1.33.0 GNOME_ORG_PVP -> PV
# removed dev-util/gtk-doc-am::gentoo from @world
# ;madhu 210313 1.33.2
# ;madhu 260125 1.35.1

EAPI=8
GNOME_ORG_MODULE="gtk-doc"

inherit gnome.org

DESCRIPTION="Automake files from gtk-doc"
HOMEPAGE="https://gitlab.gnome.org/GNOME/gtk-doc"

LICENSE="GPL-2+ FDL-1.1"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 ~sparc x86 ~arm64-macos ~x64-macos ~x64-solaris"

RDEPEND="!<dev-util/gtk-doc-${PV}"
PDEPEND="virtual/pkgconfig"

# This ebuild doesn't even compile anything, causing tests to fail when updating (bug #316071)
RESTRICT="test"

src_configure() {
	:
}

src_compile() {
	:
}

src_install() {
	insinto /usr/share/aclocal
	doins buildsystems/autotools/gtk-doc.m4
}

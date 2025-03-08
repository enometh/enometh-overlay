# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Mar 09 02:32:19 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250309 46.0: Downgrade. SRC_URI: ReUse format as babel-2.16.0.ebuild

EAPI="8"

DESCRIPTION="Unicode Common Locale Data Repository"
HOMEPAGE="https://cldr.unicode.org/"

# SRC_URI="https://unicode.org/Public/${PN#*-}/${PV%.0}/${PN#*-}-common-${PV}.zip -> ${PN}-common-${PV}.zip"
CLDR_PV=${PV}
SRC_URI="https://unicode.org/Public/cldr/${CLDR_PV%.*}/cldr-common-${CLDR_PV}.zip"

LICENSE="unicode"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 ~loong ~mips ppc ppc64 ~riscv sparc x86"
IUSE=""

RDEPEND=""
BDEPEND="app-arch/unzip"
S="${WORKDIR}"

src_install() {
	insinto /usr/share/${PN/-//}
	doins -r common
}

# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Jan 31 11:33:54 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190131 2.11-r2 - introduce prune-bindmounts patch as
# userpatch. however EAPI 5 requires overriding src_prepare.
# ;madhu 211201 2.11-r3

EAPI=7

inherit toolchain-funcs eutils

DESCRIPTION="Files which haven't been accessed are removed from specified directories"
HOMEPAGE="https://pagure.io/tmpwatch"
SRC_URI="https://releases.pagure.org/${PN}/${P}.tar.bz2"

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~alpha amd64 ~ia64 ppc ppc64 sparc x86"
IUSE="selinux"

# psmisc for fuser
DEPEND="
	!kernel_Darwin? ( sys-process/psmisc )
"
RDEPEND="
	${DEPEND}
	selinux? ( sec-policy/selinux-tmpreaper )
"

PATCHES=(
	"${FILESDIR}/${P}-boottime.patch"
	"${FILESDIR}/${P}-2.11-fix-bindmounts.patch"
)

src_compile() {
	emake AR="$(tc-getAR)"
}

src_install() {
	default

	dosbin tmpwatch
	doman tmpwatch.8

	exeinto /etc/cron.daily
	newexe "${FILESDIR}/${PN}.cron" "${PN}"
}

# Copyright 1999-2019 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

#   Time-stamp: <>
#   Touched: Thu Jan 31 11:33:54 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190131 - introduce prune-bindmounts patch as userpatch. however
# EAPI 5 requires overriding src_prepare.
# ;madhu 211201 0.26-r3 fixbindmount patch
#

EAPI=7
inherit systemd toolchain-funcs

DESCRIPTION="Merging locate is an utility to index and quickly search for files"
HOMEPAGE="https://pagure.io/mlocate"
SRC_URI="http://releases.pagure.org/mlocate/${P}.tar.xz"

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~ia64 ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86"
IUSE="nls selinux"

BDEPEND="
	acct-group/locate
	nls? ( sys-devel/gettext )
"
RDEPEND="!sys-apps/slocate
	!sys-apps/rlocate
	selinux? ( sec-policy/selinux-slocate )
"

PATCHES=(
	${FILESDIR}/mlocate-0.26-fix-bindmounts.patch
)

src_configure() {
	econf $(use_enable nls)
}

src_compile() {
	emake groupname=locate AR="$(tc-getAR)"
}

src_install() {
	emake groupname=locate DESTDIR="${D}" install
	dodoc AUTHORS ChangeLog README NEWS

	insinto /etc
	doins "${FILESDIR}"/updatedb.conf
	doins "${FILESDIR}"/mlocate-cron.conf
	fperms 0644 /etc/{updatedb,mlocate-cron}.conf

	insinto /etc/cron.daily
	newins "${FILESDIR}"/mlocate.cron-r3 mlocate
	fperms 0755 /etc/cron.daily/mlocate

	keepdir /var/lib/mlocate
	fowners 0:locate /var/lib/mlocate
	fperms 0750 /var/lib/mlocate

	systemd_dounit "${FILESDIR}"/updatedb.{service,timer}
}

pkg_postinst() {
	if [[ -z ${REPLACING_VERSIONS} ]]; then
		elog "The database for the locate command is generated daily by a cron job,"
		elog "if you install for the first time you can run the updatedb command manually now."
		elog
		elog "Note that the /etc/updatedb.conf file is generic,"
		elog "please customize it to your system requirements."
	fi
}

# Copyright 2019-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Aug 06 08:49:58 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200806 0.0.7->0.0.8
# ;madhu 240128 0.0.9
# ;madhu 250603 0.0.10 (26cc27451)
EAPI=7

USE_GIT=true

inherit systemd autotools

DESCRIPTION="libinput based console mouse daemon"
HOMEPAGE="https://salsa.debian.org/consolation-team/consolation"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI=https://salsa.debian.org/consolation-team/consolation
	EGIT_BRANCH=master
else
SRC_URI="https://salsa.debian.org/consolation-team/${PN}/-/archive/${P}/${PN}-${P}.tar.gz"
fi

LICENSE="GPL-2+ MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""

DEPEND="dev-libs/libinput:=
	virtual/libudev:=
	dev-libs/libevdev:="
RDEPEND="${DEPEND}"
BDEPEND="sys-apps/help2man"

if ! ${USE_GIT}; then
	S="${WORKDIR}/${PN}-${P}"
fi

PATCHES=(
#	"${FILESDIR}/consolation-0.0.7-makefile.patch"
)

src_prepare() {
	default

	eautoreconf
}

src_install() {
	default

	newinitd "${FILESDIR}/${PN}-initd" ${PN}
	newconfd "${FILESDIR}/${PN}-confd" ${PN}

	systemd_dounit consolation.service
}

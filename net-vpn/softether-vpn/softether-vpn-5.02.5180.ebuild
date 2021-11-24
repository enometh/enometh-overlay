# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Jan 08 10:00:25 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210108 USE_GIT=false. based on
# https://data.gpo.zugaina.org/bes/net-vpn/softether-server/softether-server-9999.ebuild
# ;madhu 211124 5.02.5180 IUSE=debug
#

EAPI=8

USE_GIT=false

inherit systemd edos2unix cmake flag-o-matic

DESCRIPTION="Multi-protocol VPN software"
HOMEPAGE="http://www.softether.org/"

MY_P=SoftEtherVPN

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="git://github.com/SoftEtherVPN/SoftEtherVPN.git"
	EGIT_SUBMODULES=(src/Mayaqua/3rdparty/cpu_features)
else
	SRC_URI="https://github.com/SoftEtherVPN/SoftEtherVPN/releases/download/5.02.5180/${MY_P}-${PV}.tar.xz"
	S=${WORKDIR}/${MY_P}-${PV}
fi


LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~amd64"
#"bridge +client cmd server"
IUSE="debug"

RDEPEND="sys-libs/ncurses:0=
	 sys-libs/readline:0=
	 dev-libs/openssl:0=
	 virtual/libiconv
		"
DEPEND="${RDEPEND}"

PATCHES=(
	"${FILESDIR}"/softether-sandbox.patch
	"${FILESDIR}"/softether-vpn-5.01.9674-debug-UDP-registrations.patch
)

src_prepare() {
	edos2unix "src/Mayaqua/Unix.c" "src/Mayaqua/Network.c"
	cmake_src_prepare
}

src_configure() {
	if use debug; then
		CMAKE_BUILD_TYPE=Debug
	fi
	cmake_src_configure
}

src_install() {
	dodoc AUTHORS.TXT README.md DISCLAIMER.md src/WARNING.TXT
	cmake_src_install
	newinitd "${FILESDIR}"/softether-server.initd softether-server
	systemd_newunit "systemd/softether-vpnserver.service" softether-server.service
}

# Copyright 1999-2024 Gentoo Authors
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
# ;madhu 240321 5.02.5182 USE_GIT=true
# ;madhu 240807 5.02.5185

EAPI=8

USE_GIT=true

inherit systemd edos2unix cmake flag-o-matic

DESCRIPTION="Multi-protocol VPN software"
HOMEPAGE="http://www.softether.org/"

MY_P=SoftEtherVPN

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/SoftEtherVPN/SoftEtherVPN.git"

	EGIT_BRANCH=madhu-tip
	EGIT_SUBMODULES=(
		src/Mayaqua/3rdparty/cpu_features
		3rdparty/tinydir
		3rdparty/BLAKE2
		src/libhamcore
		src/Mayaqua/3rdparty/oqs-provider
		src/Mayaqua/3rdparty/liboqs
	)

	EGIT_CLONE_TYPE=shallow

# the following should of course go into
# /etc/portage/env/softether-vpn-egit-override with a corresponding line
# "net-vpn/softether-vpn softether-vpn-egit-override" in
# /etc/portage/package.env.

EGIT_OVERRIDE_REPO_SOFTETHERVPN_SOFTETHERVPN=file:///14/build/SoftEtherVPN/

EGIT_OVERRIDE_REPO_GOOGLE_CPU_FEATURES=file:///14/build/SoftEtherVPN/src/Mayaqua/3rdparty/cpu_features
EGIT_OVERRIDE_REPO_CXONG_TINYDIR=file:///14/build/SoftEtherVPN/3rdparty/tinydir
EGIT_OVERRIDE_REPO_BLAKE2_BLAKE2=file:///14/build/SoftEtherVPN/3rdparty/BLAKE2
EGIT_OVERRIDE_REPO_SOFTETHERVPN_LIBHAMCORE=file:///14/build/SoftEtherVPN/src/libhamcore
EGIT_OVERRIDE_REPO_OPEN_QUANTUM_SAFE_OQS_PROVIDER=file:///14/build/SoftEtherVPN/src/Mayaqua/3rdparty/oqs-provider
EGIT_OVERRIDE_REPO_OPEN_QUANTUM_SAFE_LIBOQS=file:///14/build/SoftEtherVPN/src/Mayaqua/3rdparty/liboqs

else
	SRC_URI="https://github.com/SoftEtherVPN/SoftEtherVPN/releases/download/5.02.5182/${MY_P}-${PV}.tar.xz"
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

if ! ${USE_GIT}; then
PATCHES=(
	"${FILESDIR}"/softether-sandbox.patch
	"${FILESDIR}"/softether-vpn-5.01.9674-debug-UDP-registrations.patch
)
fi

src_prepare() {
	if ! ${USE_GIT}; then
	edos2unix "src/Mayaqua/Unix.c" "src/Mayaqua/Network.c"
	fi

	# ;madhu 240807 handle liboqs nonsense
	if ${USE_GIT}; then
		for i in `find -name CMakeLists.txt -type d`; do
			mv -fv $i $i.tmp
		done
	fi

	cmake_src_prepare

	if ${USE_GIT}; then
		for i in `find -name CMakeLists.txt.tmp -type d`; do
			mv -fv $i $(dirname $i)/$(basename $i .tmp)
		done
	fi

}

src_configure() {
	if use debug; then
		CMAKE_BUILD_TYPE=Debug
	fi
	local mycmakeargs=()
	if ${USE_GIT}; then
		mycmakeargs+=( -DBUILD_SHARED_LIBS=OFF )
	fi
	cmake_src_configure
}

src_install() {
	dodoc AUTHORS.TXT README.md DISCLAIMER.md src/WARNING.TXT
	cmake_src_install
	newinitd "${FILESDIR}"/softether-server.initd softether-server
	systemd_newunit "systemd/softether-vpnserver.service" softether-server.service
}

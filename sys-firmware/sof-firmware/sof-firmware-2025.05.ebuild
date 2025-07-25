# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2025-07-17 15:22:57 IST>
#   Touched: Thu Jul 17 15:22:02 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250717 2025.01.1 -> 2025.05

EAPI=8

DESCRIPTION="Sound Open Firmware (SOF) binary files"
HOMEPAGE="https://www.sofproject.org https://github.com/thesofproject/sof https://github.com/thesofproject/sof-bin"
SRC_URI="https://github.com/thesofproject/sof-bin/releases/download/v${PV}/sof-bin-${PV}.tar.gz -> ${P}.tar.gz"
S="${WORKDIR}"/sof-bin-${PV}

LICENSE="BSD"
SLOT="0"
KEYWORDS="amd64"
IUSE="+tools"

RDEPEND="
	tools? (
		media-libs/alsa-lib
		sys-libs/glibc
	)
"

QA_PREBUILT="usr/bin/sof-ctl
	usr/bin/sof-logger
	usr/bin/sof-probes"

src_install() {
	dodir /lib/firmware/intel
	dodir /usr/bin
	FW_DEST="${D}/lib/firmware/intel" TOOLS_DEST="${D}/usr/bin" "${S}/install.sh" || die

	# Drop tools if requested (i.e. useful for musl systems, where glibc
	# is not available)
	if ! use tools ; then
		rm -rv "${D}"/usr/bin || die
	fi
}

pkg_preinst() {
	# Fix sof-ace-tplg directory symlink collisions
	local sofpath="${EROOT}/lib/firmware/intel/sof-ace-tplg"
	if [[ ! -L "${sofpath}" && -d "${sofpath}" ]] ; then
		rm -r "${sofpath}" || die
	fi
}

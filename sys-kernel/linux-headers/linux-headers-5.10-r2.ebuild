# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2024-09-11 22:44:11 IST>
#   Touched: Fri Jan 29 17:22:11 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210129 FIXME: /usr/include/linux/kernel.h kernel-headers problem. the 5.10/00_all_0009_glibc-specific-inclusion-of-sysinfo.h-in-kernel.h.patch hadn't applied cleanly with 5.10.11 and was not included

EAPI=8

USE_GIT=true

ETYPE="headers"
H_SUPPORTEDARCH="alpha amd64 arc arm arm64 avr32 cris frv hexagon hppa ia64 m68k metag microblaze mips mn10300 nios2 openrisc ppc ppc64 riscv s390 score sh sparc x86 xtensa"
inherit kernel-2
detect_version

PATCH_PV=${PV} # to ease testing new versions against not existing patches
PATCH_VER="1"

SRC_URI="${KERNEL_URI}
	${PATCH_VER:+mirror://gentoo/gentoo-headers-${PATCH_PV}-${PATCH_VER}.tar.xz}
"
if ${USE_GIT}; then
S=${WORKDIR}/${P}
else
S="${WORKDIR}/linux-${PV}"
fi

#	${PATCH_VER:+mirror://gentoo/gentoo-headers-${PATCH_PV}-${PATCH_VER}.tar.xz}
#	${PATCH_VER:+https://dev.gentoo.org/~slyfox/distfiles/gentoo-headers-${PATCH_PV}-${PATCH_VER}.tar.xz}"

if ${USE_GIT}; then
	SRC_URI=""
	inherit git-r3
	EGIT_REPO_URI="file:///build/git-mirror/linux40.git"
	EGIT_BRANCH=tmp-madhu-5.10.225
	EGIT_CLONE_TYPE=shallow
fi

KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux"

BDEPEND="
	app-arch/xz-utils
	dev-lang/perl"

# [[ -n ${PATCH_VER} ]] && PATCHES=( "${WORKDIR}"/${PATCH_PV} )

src_unpack() {
	# avoid kernel-2_src_unpack
if ${USE_GIT}; then
		git-r3_src_unpack
else
	default
fi

}

src_prepare() {
	# TODO: May need forward porting to newer versions
	use elibc_musl && PATCHES+=(
		"${FILESDIR}"/${PN}-5.10-Use-stddefs.h-instead-of-compiler.h.patch
	)

	# avoid kernel-2_src_prepare
	default
}

src_test() {
	emake headers_check "${KERNEL_MAKEOPTS[@]}"
}

src_install() {
	kernel-2_src_install

	find "${ED}" \( -name '.install' -o -name '*.cmd' \) -delete || die
	# delete empty directories
	find "${ED}" -empty -type d -delete || die
}

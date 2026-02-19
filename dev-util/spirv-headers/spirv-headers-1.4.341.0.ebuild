# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Sep 30 12:29:20 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200930 1.5.3 -> 1.5.3.reservations
# ;madhu 210301 1.5.4.1
# ;madhu 220221 1.3.204 --> version number rolledback
# ;madhu 250718 1.4.321.0 - change name of file downloaded to DISTDIR to be different from that in gentoo. 2. have to older mask binary packages for emerge -K.
# ;madhu 260220 1.4.341.0
EAPI=8

inherit cmake

DESCRIPTION="Machine-readable files for the SPIR-V Registry"
HOMEPAGE="https://www.khronos.org/registry/spir-v/"
SRC_URI="https://github.com/KhronosGroup/SPIRV-Headers/archive/vulkan-sdk-${PV}.tar.gz -> SPIRV-Headers-vulkan-sdk-${PV}.tar.gz"
S="${WORKDIR}"/SPIRV-Headers-vulkan-sdk-${PV}
LICENSE="MIT"
SLOT="0"
KEYWORDS="~alpha amd64 ~arm ~arm64 ~hppa ~loong ~mips ppc ppc64 ~riscv ~s390 ~sparc x86"

src_configure() {
	local mycmakeargs=(
		-DSPIRV_HEADERS_ENABLE_TESTS=OFF
		-DSPIRV_HEADERS_ENABLE_INSTALL=ON
	)
	cmake_src_configure
}

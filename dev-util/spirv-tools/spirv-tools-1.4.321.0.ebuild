# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2022-02-21 20:27:26 IST>
#   Touched: Wed Sep 30 11:43:35 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200930 2020.3 -> 2020.5
# ;madhu 220221 1.3.204 -> 1.3.206
# ;madhu 250718 1.3.206 -> 1.4.321.0

EAPI=8

PYTHON_COMPAT=( python3_{11..14} )
PYTHON_REQ_USE="xml(+)"
inherit cmake-multilib python-any-r1

SRC_URI="https://github.com/KhronosGroup/SPIRV-Tools/archive/vulkan-sdk-${PV}.tar.gz -> SPIRV-Tools-vulkan-sdk-${PV}.tar.gz"
KEYWORDS="~alpha amd64 ~arm ~arm64 ~hppa ~loong ~mips ppc ppc64 ~riscv ~s390 ~sparc x86"
S="${WORKDIR}"/SPIRV-Tools-vulkan-sdk-${PV}
DESCRIPTION="Provides an API and commands for processing SPIR-V modules"
HOMEPAGE="https://github.com/KhronosGroup/SPIRV-Tools"

LICENSE="Apache-2.0"
SLOT="0"
IUSE="test"
RESTRICT="!test? ( test )"

DEPEND="~dev-util/spirv-headers-${PV}"
# RDEPEND=""
BDEPEND="${PYTHON_DEPS}"

multilib_src_configure() {
	local mycmakeargs=(
		-DSPIRV-Headers_SOURCE_DIR="${ESYSROOT}"/usr/
		-DSPIRV_WERROR=OFF
		-DSPIRV_SKIP_TESTS=$(usex !test)
		-DSPIRV_TOOLS_BUILD_STATIC=OFF
		-DCMAKE_C_FLAGS="${CFLAGS} -DNDEBUG"
		-DCMAKE_CXX_FLAGS="${CXXFLAGS} -DNDEBUG"
	)

	cmake_src_configure
}

src_test() {
	CMAKE_SKIP_TESTS=(
		# Not relevant for us downstream
		spirv-tools-copyrights
		# Tests fail upon finding symbols that do not match a regular expression
		# in the generated library. Easily hit with non-standard compiler flags
		spirv-tools-symbol-exports.*
	)

	multilib-minimal_src_test
}

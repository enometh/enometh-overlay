# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2021-09-10 05:54:41 IST>
#   Touched: Fri Sep 10 05:44:04 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210910 12.0.0 -> 13.0.0
# ;madhu 250718 21.0.8 -- build with /opt/llvm-20.1.8 (hardcoded) without gentoo llvm eclasses, fix SRC_URI name, FIXME: installs doc under /usr

EAPI=8

LLVM_COMPAT=( 20 )

inherit cmake-multilib flag-o-matic llvm-r2 multiprocessing

DESCRIPTION="Bi-directional translator between SPIR-V and LLVM IR"
HOMEPAGE="https://github.com/KhronosGroup/SPIRV-LLVM-Translator"

SRC_URI="https://github.com/KhronosGroup/SPIRV-LLVM-Translator/archive/v${PV}.tar.gz -> SPIRV-LLVM-Translator-${PV}.tar.gz"
S=${WORKDIR}/SPIRV-LLVM-Translator-${PV}

MULTILIB_ABIS="amd64"

LICENSE="UoI-NCSA"
SLOT="$(ver_cut 1)"
KEYWORDS="amd64 ~arm64 ~loong ~riscv x86"
IUSE="test"
RESTRICT="!test? ( test )"

# 	llvm-core/llvm:${SLOT}=[${MULTILIB_USEDEP}]
RDEPEND="
	dev-util/spirv-tools[${MULTILIB_USEDEP}]
"

#;madhu 250718 - enometh-overlay
DEPEND="${RDEPEND}
	>=dev-util/spirv-headers-1.4.321.0
"
BDEPEND="
	virtual/pkgconfig
	test? (
		dev-python/lit
		llvm-core/clang:${SLOT}
	)
"

src_prepare() {
#	PATH=/opt/llvm-20.1.8/bin:$PATH
	default
}

get_llvm_prefix() {
	echo "${ESYSROOT}/opt/llvm-20.1.8/"
}

#PATCHES=( "${FILESDIR}"/${PN}-20.1.3-option-registered.patch )

src_prepare() {
	append-flags -fPIC
	cmake_src_prepare

	# do not force a specific LLVM version to find_package(), this only
	# causes issues and we force a specific path anyway
	sed -i -e '/find_package/s:${BASE_LLVM_VERSION}::' CMakeLists.txt || die
}

multilib_src_configure() {
	local mycmakeargs=(
		-DCCACHE_ALLOWED="OFF"
		-DCMAKE_INSTALL_PREFIX="$(get_llvm_prefix)"
		-DLLVM_EXTERNAL_SPIRV_HEADERS_SOURCE_DIR="${ESYSROOT}/usr/include/spirv"
		-DLLVM_SPIRV_INCLUDE_TESTS=$(usex test "ON" "OFF")
		-Wno-dev
	)

	cmake_src_configure
}

multilib_src_test() {
	lit -vv "-j${LIT_JOBS:-$(makeopts_jobs)}" "${BUILD_DIR}/test" || die
}

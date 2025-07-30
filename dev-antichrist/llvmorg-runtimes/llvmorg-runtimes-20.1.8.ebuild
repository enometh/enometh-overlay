# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Jul 17 10:24:27 2025 -0600 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250730 - 20.1.8 under /opt/llvm-20.1.8, for poc only. the runtimes are bundled with dev-antichrist/llvmorg.

EAPI=8

PYTHON_COMPAT=( python3_{10..13} )

#inherit cmake
inherit ninja-utils
inherit prefix python-single-r1 toolchain-funcs

DESCRIPTION="New implementation of the C++ standard library and runtimes" #rly?
HOMEPAGE="https://llvm.org/"

LICENSE="Apache-2.0-with-LLVM-exceptions UoI-NCSA MIT"
SRC_URI="github.com/llvm/llvm-project/archive/refs/tags/llvmorg-${PV}.tar.gz"
SLOT=20/20.1
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86 ~amd64-linux ~arm64-macos ~x64-macos"

REQUIRED_USE="${PYTHON_REQUIRED_USE}"
# bogus
IUSE="abi_x86_32 +atomic-builtins +clang +ctx-profile +debug +libcxxabi +libfuzzer +memprof +orc +profile +static-libs +xray abi_x86_64 debug gdb-plugin hwloc llvm_targets_AMDGPU llvm_targets_NVPTX ompt static-libs test"
S="${WORKDIR}/llvm-project-llvmorg-${PV}"

# provides llvm, clang
DEPEND="
	dev-antichrist/llvmorg
"

RDEPEND="
	${PYTHON_DEPS}
	${DEPEND}
"
BDEPEND="
	dev-build/cmake
	${NINJA_DEPEND}
	${PYTHON_DEPS}
"

src_prepare() {
	: ${BUILD_DIR:=${WORKDIR}/${P}_build}
	mkdir -pv $BUILD_DIR
	default_src_prepare
}

src_configure() {
	local llvm_prefix=${EPREFIX}/opt/llvm-${PV}
	local mycmakeargs=(
#		-DDEFAULT_SYSROOT="${EPREFIX}"
		-DCMAKE_INSTALL_PREFIX="${llvm_prefix}"

#		-DLLVM_DIR=${llvm_prefix}/lib/cmake/llvm
#		-DClang_DIR=${llvm_prefix}/lib/cmake/clang

		-DCMAKE_C_COMPILER="clang"
		-DCMAKE_CXX_COMPILER="clang++"

		-DBUILD_SHARED_LIBS=ON
		-DLLVM_INCLUDE_DOCS=OFF
		-DLLVM_INCLUDE_TESTS=OFF
		-DCMAKE_BUILD_TYPE=Release

		-G Ninja
		-DLLVM_LIBDIR_SUFFIX=64
		-DLIBCXXABI_LIBDIR_SUFFIX=64
		-DLIBCXX_LIBDIR_SUFFIX=64
		-DLIBUNWIND_LIBDIR_SUFFIX=64

		-DLLVM_ENABLE_RUNTIMES="libc;libunwind;libcxxabi;libcxx;compiler-rt;openmp"
		-DOPENMP_TEST_ENABLE_TSAN=0
		-DCMAKE_BUILD_TYPE=$(usex debug Debug Release)

		-DLIBOMP_USE_HWLOC=$(usex hwloc)
		-DLIBOMP_OMPD_GDB_SUPPORT=$(multilib_native_usex gdb-plugin)
		-DLIBOMP_OMPT_SUPPORT=$(usex ompt)

		-DPython3_EXECUTABLE="${PYTHON}"
		-DCLANG_RESOURCE_DIR=../lib/clang/20
	)

	cmake -S ${S}/runtimes -B ${BUILD_DIR} "${mycmakeargs[@]}"
}
src_compile () {
	#;madhu 250717 todo use cmake --build ${BUILD_DIR}
	ninja -C ${BUILD_DIR}
}

src_install() {
	# cmake --install ${BUILD_DIR} --prefix ${D}/opt/llvm-${PV}
	DESTDIR="${D}" \
		   ninja -C ${BUILD_DIR} install
}

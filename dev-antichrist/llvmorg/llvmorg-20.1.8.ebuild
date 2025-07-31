# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Jul 17 10:24:27 2025 -0600 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250717 - 20.1.8 - try to bypass gentoo llvm. install llvm,
# clang, libclc under /opt/llvm-${PV} using the llvmorg tarball. use
# profile/package.provided

EAPI=8

PYTHON_COMPAT=( python3_{10..13} )

#inherit cmake
inherit ninja-utils
inherit prefix python-single-r1 toolchain-funcs

DESCRIPTION="C language family frontend for LLVM"
HOMEPAGE="https://llvm.org/"

# MSVCSetupApi.h: MIT
# sorttable.js: MIT

LICENSE="Apache-2.0-with-LLVM-exceptions UoI-NCSA MIT"
SRC_URI="github.com/llvm/llvm-project/archive/refs/tags/llvmorg-${PV}.tar.gz"
SLOT=20/20.1
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86 ~amd64-linux ~arm64-macos ~x64-macos"
IUSE="debug doc +extra ieee-long-double +pie +static-analyzer xml"
REQUIRED_USE="${PYTHON_REQUIRED_USE}"

S="${WORKDIR}/llvm-project-llvmorg-${PV}"

DEPEND="
	static-analyzer? ( dev-lang/perl:* )
	xml? ( dev-libs/libxml2:2= )
"

RDEPEND="
	${PYTHON_DEPS}
	${DEPEND}
"
BDEPEND="
	dev-build/cmake
	${NINJA_DEPEND}
	${PYTHON_DEPS}
	xml? ( virtual/pkgconfig )
"
src_prepare() {
	: ${BUILD_DIR:=${WORKDIR}/${P}_build}
	mkdir -pv $BUILD_DIR
	default_src_prepare
}

# www.llvm.org/docs/CMake.html

src_configure() {
	local llvm_prefix=${EPREFIX}/opt/llvm-${PV}
	local mycmakeargs=(
		-DDEFAULT_SYSROOT="${EPREFIX}"
		-DCMAKE_INSTALL_PREFIX="${llvm_prefix}"

# set(LLVM_ALL_PROJECTS "bolt;clang;clang-tools-extra;compiler-rt;cross-project-tests;libc;libclc;lld;lldb;mlir;openmp;polly;pstl")

		# ;openmp;clang-tools-extra;openmp
		-DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra;lld;lldb;libclc"
		-DLLVM_TARGETS_TO_BUILD="X86;AMDGPU;BPF"
		-DLLVM_ENABLE_RUNTIMES="libc;libunwind;libcxxabi;libcxx;compiler-rt;openmp"

		# XXX
		-DLIBCLC_TARGETS_TO_BUILD="amdgcn-mesa-mesa3d"

		-DBUILD_SHARED_LIBS=OFF
		-DLLVM_LINK_LLVM_DYLIB=ON
		-DCLANG_LINK_CLANG_DYLIB=ON

		-DLLVM_LIBDIR_SUFFIX=64
		-GNinja

		-DBENCHMARK_ENABLE_ASSEMBLY_TESTS=OFF
		-DBENCHMARK_INSTALL_DOCS=OFF
		-DCLANG_INCLUDE_DOCS=OFF
		-DCLANG_INCLUDE_TESTS=OFF
		-DCLANG_TOOL_APINOTES_TEST_BUILD=OFF
		-DCLANG_TOOL_ARCMT_TEST_BUILD=OFF
		-DCLANG_TOOL_CLANG_IMPORT_TEST_BUILD=OFF
		-DCLANG_TOOL_C_ARCMT_TEST_BUILD=OFF
		-DCLANG_TOOL_C_INDEX_TEST_BUILD=OFF
		-DLLVM_INCLUDE_DOCS=OFF
		-DLLVM_INCLUDE_TESTS=OFF
		-DLLVM_TOOL_LLVM_C_TEST_BUILD=OFF
		-DCMAKE_BUILD_TYPE=$(usex debug Debug Release)

#		# these are not propagated reliably, so redefine them
		-DLLVM_ENABLE_EH=ON
		-DLLVM_ENABLE_RTTI=ON

#		-DCLANG_ENABLE_LIBXML2=$(usex xml)
#		-DCLANG_ENABLE_ARCMT=$(usex static-analyzer)
#		-DCLANG_DEFAULT_PIE_ON_LINUX=$(usex pie)
#		-DCLANG_ENABLE_STATIC_ANALYZER=$(usex static-analyzer)
		-DPython3_EXECUTABLE="${PYTHON}"
	)

	# LLVM can have very high memory consumption while linking,
	# exhausting the limit on 32-bit linker executable
	use x86 && local -x LDFLAGS="${LDFLAGS} -Wl,--no-keep-memory"

	use debug || local -x CPPFLAGS="${CPPFLAGS} -DNDEBUG"
	cmake -S ${S}/llvm -B ${BUILD_DIR} "${mycmakeargs[@]}"
}
src_compile () {
	#;madhu 250717 todo use cmake --build ${BUILD_DIR}
	ninja -C ${BUILD_DIR}
}

src_install() {
	# cmake --install ${BUILD_DIR} --prefix ${D}/opt/llvm-${PV}
	DESTDIR="${D}" \
		   ninja -C ${BUILD_DIR} install

	local llvm_prefix=${EPREFIX}/opt/llvm-${PV}
#	ln -sv lib ${ED}/opt/${llvm_prefix}/lib64
#	ln -sv ../ ${ED}/opt/llvm-20.1.8/{share/pkgconfig,lib64/}

	# ;madhu 250730 CHECK this numbering scheme. scheme. this has to be
	# ;a lower number than the other llvm slots.
	local revord=$(( 9999 - 20 ))
	newenvd - "60llvm-${revord}" <<-_EOF_
		PATH="${llvm_prefix}/bin"
		ROOTPATH="${llvm_prefix}/bin"
		LDPATH="${llvm_prefix}/lib"
	_EOF_

}

# Copyright 2021-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Nov 09 11:27:08 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020-2022 Madhu.  All Rights Reserved.
#
# ;madhu 201109 0.1 llvm10
# ;madhu 211124 1.0.0_rc1 llvm13 v1.0.0-rc1-31-g5636ea6
# ;madhu 221128 1.0.0_rc1-r2 llvm14 v1.0.0-rc1-35-g76ce56d
# ;madhu 230110 1.0.0_rc1-r3 ditto but use cmake-multilib

EAPI=8

inherit cmake-multilib llvm
USE_GIT=true

MY_COMMIT="305cf234b9ade75d2ffcee66bf88563ba1e85d1a"

DESCRIPTION="Simple-ABI Wrapper over libclang for generating Common Lisp bindings"
HOMEPAGE="https://github.com/borodust/libresect"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/borodust/libresect"
	EGIT_BRANCH="master"
	EGIT_COMMIT="$MY_COMMIT"
	EGIT_CLONE_TYPE="shallow"
	EGIT_SUBMODULES=()
else
	SRC_URI="https://github.com/borodust/libresect/archive/${MY_COMMIT}.tar.gz -> ${P}.tar.gz"
	S="${WORKDIR}/${PN}-${MY_COMMIT}"
fi

LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""

LLVM_MAX_SLOT=14
SLOT="14"

RDEPEND="sys-devel/clang:${SLOT}=
	sys-devel/llvm:${SLOT}=
"
DEPEND="${DEPEND}"

DOCS=( README.md LICENSE )

PATCHES=(
	$FILESDIR/libresect-1.0.0_rc1-LLVM-13-beta.patch
	$FILESDIR/libresect-1.0.0_rc1-r3-CMakeLists-non-static-lib64.patch
	$FILESDIR/libresect-1.0.0_rc1-r3-src_test_fix-tests.patch
)

pkg_setup() {
	llvm_pkg_setup
}

multilib_src_configure() {
	einfo "Forcing the use of clang ..."
	export CC="${CHOST}-clang"
	export CXX="${CHOST}-clang++"
	cmake_src_configure
}

multilib_src_install() {
	cmake_src_install
	if ! multilib_is_native_abi; then
#		mv -iv ${ED}/usr/bin/resect-test{,_i686}
		rm -fv ${ED}/usr/bin/resect-test
	fi
}

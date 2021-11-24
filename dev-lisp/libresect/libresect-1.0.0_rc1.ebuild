# Copyright 2021-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Nov 09 11:27:08 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 201109 0.1 llvm10
# ;madhu 211124 1.0.0_rc1 llvm13 v1.0.0-rc1-31-g5636ea6

EAPI=7

inherit cmake llvm
USE_GIT=true

MY_COMMIT="51effa8d22f5e52804f20c4db289f331205cc00e"

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
KEYWORDS="~amd64"
IUSE=""

RDEPEND="sys-devel/clang:13=
	sys-devel/llvm:13=
"
DEPEND="${DEPEND}"

PATCHES=(
	$FILESDIR/libresect-1.0.0_rc1-LLVM-13-beta.patch
	$FILESDIR/libresect-1.0.0_rc1-CMakeLists-non-static-lib64.patch
)

pkg_setup() {
	LLVM_MAX_SLOT=13 llvm_pkg_setup
}

src_configure() {
	einfo "Forcing the use of clang ..."
	CC=${CHOST}-clang
	CXX=${CHOST}-clang++
	cmake_src_configure
}

src_install() {
	dodoc README.md LICENSE
	cmake_src_install
}

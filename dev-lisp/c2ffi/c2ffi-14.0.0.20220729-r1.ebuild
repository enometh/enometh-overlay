# Copyright 2019-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Jan 14 20:57:49 2019 -0700 <madhu@cs.unm.edu>
#   Bugs-To: madhu@cs.unm.edu
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2022 Madhu.  All Rights Reserved.
#
#; madhu 2019-01-14 0.0-r1
#; madhu 190806  0.0-r2 llvm8 - no patches
#; madhu 201001 c2ffi-10.0.0.20200527.ebuild -> 20200721
#; madhu 211124 c2ffi-13.0.0.20211010.ebuild - 12.0.0 patched for llvm13
#; madhu 221128 c2ffi-14.0.0.20220729-r1.ebuild - same as gentoo

EAPI=8

LLVM_MAX_SLOT=14
inherit cmake llvm

MY_COMMIT="25fcec13381f495460f4a4eafdd1b939c799df4a"
USE_GIT=true

DESCRIPTION="Clang-based FFI wrapper generator for Common Lisp"
HOMEPAGE="https://github.com/rpav/c2ffi"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/rpav/c2ffi"
	EGIT_BRANCH="llvm-14.0.0"	# make sure it exists
	EGIT_COMMIT="$MY_COMMIT"
#	EGIT_CLONE_TYPE="shallow"
else
	SRC_URI="https://github.com/rpav/c2ffi/archive/${MY_COMMIT}.tar.gz -> ${P}.tar.gz"
	S="${WORKDIR}/${PN}-${MY_COMMIT}"
fi

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="~amd64"
IUSE=""

DEPEND="sys-devel/clang:${LLVM_MAX_SLOT}
	sys-devel/llvm:${LLVM_MAX_SLOT}"
RDEPEND="${DEPEND}"


src_install() {
	dodoc AUTHORS README.md
	cmake_src_install
}

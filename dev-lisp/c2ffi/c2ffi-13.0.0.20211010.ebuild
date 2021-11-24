# Distributed under the terms of the GNU General Public License v2
# Copyright 1999-2022 Gentoo Authors
#
#   Time-stamp: <>
#   Touched: Mon Jan 14 20:57:49 2019 -0700 <madhu@cs.unm.edu>
#   Bugs-To: madhu@cs.unm.edu
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
#; madhu 2019-01-14 0.0-r1
#; madhu 190806  0.0-r2 llvm8 - no patches
#; madhu 201001 c2ffi-10.0.0.20200527.ebuild -> 20200721
#; madhu 211124 c2ffi-13.0.0.20211010.ebuild - 12.0.0 patched for llvm13

EAPI=7

inherit cmake llvm

MY_COMMIT="cedddbb5da39d92ae20787bb5f398bf284d6089f"
USE_GIT=true

DESCRIPTION="Clang-based FFI wrapper generator for Common Lisp"
HOMEPAGE="https://github.com/rpav/c2ffi"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/rpav/c2ffi"
	EGIT_BRANCH="llvm-12.0.0"	# make sure it exists
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

DEPEND="sys-devel/clang:13=
	sys-devel/llvm:13=
"
RDEPEND="${DEPEND}"

#S="${WORKDIR}/c2ffi-${PV}"

PATCHES=(
"$FILESDIR/c2ffi-10.0.0-add-target-link-directories.patch"
"$FILESDIR/c2ffi-12.0.0-llvm13.patch"
)


src_install() {
	dodoc AUTHORS README.md
	cmake_src_install
}

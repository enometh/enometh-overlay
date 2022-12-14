# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Jan 01 17:10:09 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
#;madhu 210101 1.48 force binary package
#;madhu 210824 1.54
#;madhu 220311 1.59
#;madhu 221214 1.65.0-r1 - USE=-abi_x86_32, drop subslot

EAPI=8

inherit multilib-build

DESCRIPTION="Virtual for Rust language compiler"

LICENSE=""

# adjust when rust upstream bumps internal llvm
# we do not allow multiple llvm versions in dev-lang/rust for
# neither system nor bundled, so we just hardcode it here.
#SLOT="0/llvm-15"
#;madhu 221214
SLOT="0"
KEYWORDS="amd64 arm arm64 ~mips ppc ppc64 ~riscv ~s390 sparc x86"
IUSE="rustfmt"

BDEPEND=""
RDEPEND="|| (
	dev-lang/rust[rustfmt?,${MULTILIB_USEDEP}]
	dev-lang/rust-bin[rustfmt?,${MULTILIB_USEDEP}]
)"

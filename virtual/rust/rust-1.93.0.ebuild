# Copyright 1999-2026 Gentoo Authors
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
#;madhu 260126 1.93.0  catch up with x86 again
EAPI=8

inherit multilib-build

DESCRIPTION="Virtual for Rust language compiler"

LICENSE=""

# adjust when rust upstream bumps internal llvm
# we do not allow multiple llvm versions in dev-lang/rust for
# neither system nor bundled, so we just hardcode it here.
#;madhu 221214
# SLOT="0"
SLOT="0/llvm-21"
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86"
IUSE="rustfmt profiler"

RDEPEND="|| (
	~dev-lang/rust-bin-${PV}[rustfmt?,${MULTILIB_USEDEP}]
	~dev-lang/rust-${PV}[profiler?,rustfmt?,${MULTILIB_USEDEP}]
)"

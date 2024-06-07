# Copyright 2019-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Apr 25 19:35:46 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210425 0.11.4-r1 -> 0.12.1, the install_lib patches are updated
# but wont work: src/bpftrace -e 'BEGIN { @++ }' since libbpftrace.so
# has the symbol -DBUILD_SHARED_LIB=off
#
# ;madhu 221110 0.16.0 unkeyworded in gentoo but works with installed
# libbpf, drop all patches
# ;madhu 221214 LLVM14

EAPI=8

LLVM_MAX_SLOT=14

inherit llvm linux-info cmake

DESCRIPTION="High-level tracing language for eBPF"
HOMEPAGE="https://github.com/iovisor/bpftrace"
MY_PV="${PV//_/}"
KEYWORDS="~amd64 ~arm64 ~x86"

SRC_URI="https://github.com/iovisor/${PN}/archive/v${MY_PV}.tar.gz -> ${P}.gh.tar.gz"
S="${WORKDIR}/${PN}-${MY_PV:-${PV}}"

LICENSE="Apache-2.0"
SLOT="0"

IUSE="fuzzing test"

# lots of fixing needed
RESTRICT="test"

RDEPEND="
	>=dev-libs/libbpf-0.8.1:=
	>=dev-util/bcc-0.25.0:=
	dev-util/systemtap
	<sys-devel/clang-$((${LLVM_MAX_SLOT} + 1)):=
	<sys-devel/llvm-$((${LLVM_MAX_SLOT} + 1)):=[llvm_targets_BPF(+)]
	sys-libs/binutils-libs:=
	virtual/libelf:=
"
DEPEND="
	${COMMON_DEPEND}
	dev-libs/cereal:=
	test? ( dev-cpp/gtest )
"
BDEPEND="
	sys-apps/sed
	app-arch/xz-utils
	sys-devel/flex
	sys-devel/bison
	virtual/pkgconfig
"

QA_DT_NEEDED="
	/usr/lib.*/libbpftraceresources.so
	/usr/lib.*/libcxxdemangler_llvm.so
"

PATCHES=(
#	"${FILESDIR}/bpftrace-0.16.0-install-libs.patch"
#	"${FILESDIR}/bpftrace-0.15.0-dont-compress-man.patch"
#	"${FILESDIR}/bpftrace-0.11.4-old-kernels.patch"
)

pkg_pretend() {
	local CONFIG_CHECK="
		~BPF
		~BPF_EVENTS
		~BPF_JIT
		~BPF_SYSCALL
		~FTRACE_SYSCALLS
		~HAVE_EBPF_JIT
	"

	check_extra_config
}

pkg_setup() {
	llvm_pkg_setup
}

src_configure() {
	local -a mycmakeargs=(
		-DSTATIC_LINKING:BOOL=OFF
		"-DBUILD_SHARED_LIBS:=OFF"
		# bug 809362, 754648
		-DBUILD_TESTING:BOOL=$(usex test)
		-DBUILD_FUZZ:BOOL=$(usex fuzzing)
		-DENABLE_MAN:BOOL=OFF
	)

	cmake_src_configure
}

src_install() {
	cmake_src_install
	# bug 809362
	dostrip -v -p -x ${ED}/usr/bin/bpftrace
	doman man/man8/*.?
	dodoc docs/*.md
}

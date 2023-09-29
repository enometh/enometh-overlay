# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Apr 15 17:26:40 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021-2023 Madhu.  All Rights Reserved.
#
# ;madhu 210415 binary dist 21.14.19498.ebuild
# ;madhu 211017 21.41.21220.ebuild
#
# ;madhu 230929 23.30.26918.9 renamed fromdev-libs/intel-neo-bin::localrepo
#  skip#  gmmlib and debugsysms debs. ignores USE flags (assumes they are set).
#

EAPI=8

inherit unpacker

MY_PN="compute-runtime"
MY_P="${MY_PN}-${PV}"

DESCRIPTION="Intel Graphics Compute Runtime for oneAPI Level Zero and OpenCL Driver"
HOMEPAGE="https://github.com/intel/compute-runtime"
SRC_URI="
https://github.com/intel/intel-graphics-compiler/releases/download/igc-1.0.14828.8/intel-igc-core_1.0.14828.8_amd64.deb
https://github.com/intel/intel-graphics-compiler/releases/download/igc-1.0.14828.8/intel-igc-opencl_1.0.14828.8_amd64.deb
https://github.com/intel/compute-runtime/releases/download/23.30.26918.9/intel-level-zero-gpu_1.3.26918.9_amd64.deb
https://github.com/intel/compute-runtime/releases/download/23.30.26918.9/intel-opencl-icd_23.30.26918.9_amd64.deb
"

#https://github.com/intel/compute-runtime/releases/download/23.30.26918.9/intel-level-zero-gpu-dbgsym_1.3.26918.9_amd64.ddeb
#https://github.com/intel/compute-runtime/releases/download/23.30.26918.9/intel-opencl-icd-dbgsym_23.30.26918.9_amd64.ddeb
# https://github.com/intel/compute-runtime/releases/download/23.30.26918.9/libigdgmm12_22.3.0_amd64.deb

LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64"
IUSE="l0 vaapi"

RDEPEND=">=media-libs/gmmlib-22.3.5:="

# COMMON=">=media-libs/gmmlib-22.3.5:=
# 	dev-libs/intel-metrics-discovery:=
# 	dev-libs/intel-metrics-library:=
# 	dev-libs/libnl:3
# 	dev-libs/libxml2:2
# 	>=dev-util/intel-graphics-compiler-1.0.14062.11
# 	>=dev-util/intel-graphics-system-controller-0.8.9:=
# 	media-libs/mesa
# 	>=virtual/opencl-3
# 	l0? ( >=dev-libs/level-zero-1.13.1:= )
# 	vaapi? (
# 		x11-libs/libdrm[video_cards_intel]
# 		media-libs/libva
# 	)
# "

#mesa  for Khronos OpenGL headers

DEPEND="${COMMON}
	>=virtual/opencl-3
	media-libs/mesa
	l0? ( >=dev-libs/level-zero-1.13.1:= )
	vaapi? (
		x11-libs/libdrm[video_cards_intel]
		media-libs/libva
	)
"

#PATCHES=(
#)

#DOCS=(
#	README.md
#	FAQ.md
#)

S="${WORKDIR}"

src_unpack() {
	:
}

src_install() {
	dodir /
	cd "${ED}" || die
	unpacker
	mv -v  usr/local/lib usr/lib64 || die
	rmdir -v usr/local || die
	mv -v  usr/lib/x86_64-linux-gnu/* usr/lib64 || die
	rmdir -v usr/lib/x86_64-linux-gnu/ || die
	echo /usr/lib64/intel-opencl/libigdrcl.so > etc/OpenCL/vendors/intel.icd
	gunzip -v usr/share/doc/*/*
	mkdir -pv usr/share/doc/${P}
	mv -iv usr/share/doc/* usr/share/doc/${P} # let GNU mv handle the same src-target..
}

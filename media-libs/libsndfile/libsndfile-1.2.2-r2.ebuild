# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Jan 09 05:25:46 2020 +0000 <madhu@cs.unm.edu>
#   Bugs-To: madhu@cs.unm.edu
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 200109 - no autogen FIXME -move build to cmake system which
# won't break when calling autogen autoconf has HAVE_AUTOGEN but
# makefile doesn't know how to use it to avoid calling autogen even when
# tests are not required
#
# ;madhu 250311 - depend directly on media-sound/mpg123 not
# media-sound/mpg123-base

EAPI=8

PYTHON_COMPAT=( python3_{10..13} pypy3 )

if [[ ${PV} == *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/libsndfile/libsndfile.git"
else
#   SRC_URI="https://github.com/erikd/libsndfile/archive/${MY_COMMIT}.tar.gz -> ${P}.tar.gz"
	SRC_URI="https://github.com/libsndfile/libsndfile/releases/download/${PV}/${P}.tar.xz"
	KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~x64-solaris"
fi
inherit flag-o-matic python-any-r1 cmake-multilib # multilib-minimal

DESCRIPTION="C library for reading and writing files containing sampled sound"
HOMEPAGE="https://libsndfile.github.io/libsndfile/"

LICENSE="LGPL-2.1"
SLOT="0"
IUSE="alsa minimal sqlite test"
RESTRICT="!test? ( test )"

# 		media-sound/mpg123-base:=[${MULTILIB_USEDEP}]
RDEPEND="
	!minimal? (
		media-libs/flac:=[${MULTILIB_USEDEP}]
		media-libs/libogg:=[${MULTILIB_USEDEP}]
		media-libs/libvorbis:=[${MULTILIB_USEDEP}]
		media-libs/opus:=[${MULTILIB_USEDEP}]
		media-sound/lame:=[${MULTILIB_USEDEP}]
		media-sound/mpg123:=[${MULTILIB_USEDEP}]
	)
	alsa? ( media-libs/alsa-lib:= )
	sqlite? ( dev-db/sqlite )"
DEPEND="${RDEPEND}"
BDEPEND="
	${PYTHON_DEPS}
	virtual/pkgconfig"
if [[ ${PV} == *9999 ]]; then
#;madhu 200109 Behold an Israelite indeed, in whom is no guile!
#	BDEPEND+="
#		sys-devel/autogen
#	"
	:
fi

src_prepare() {
	cmake_src_prepare
}

#src_configure() {
#	# https://github.com/libsndfile/libsndfile/issues/1049 (bug #943864)
#	append-cflags -std=gnu17
#
#	multilib_src_configure
#}

multilib_src_configure() {
	append-cflags -std=gnu17
	local mycmakeargs=(
#		--disable-octave \
#		--disable-static \
#		--disable-werror \

		"-DENABLE_EXTERNAL_LIBS=$(usex !minimal)"
		"-DENABLE_MPEG=$(usex !minimal)"

#		$(multilib_native_enable full-suite) \
#		$(multilib_native_use_enable alsa) \
#		$(multilib_native_use_enable sqlite) \
#		PYTHON="${EPYTHON}"
	)

	cmake_src_configure
}

multilib_src_install_all() {
	einstalldocs

	# no static archives
#	find "${ED}" -name '*.la' -delete || die
}

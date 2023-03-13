# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Sep 23 12:33:31 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210923 1.7.1-r2 -> 1.7.4
# ;madhu 220727 1.9.0
# ;madhu 230109 1.10.0
# ;madhu 230313 1.11.0, fix slot on rsvg

EAPI=8

USE_GIT=true

# Must be bumped with media-plugins/imlib2_loaders!

inherit autotools multilib-minimal toolchain-funcs

DESCRIPTION="Version 2 of an advanced replacement library for libraries like libXpm"
HOMEPAGE="https://www.enlightenment.org/
	https://sourceforge.net/projects/enlightenment/files/imlib2-src/"
if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://git.enlightenment.org/old/legacy-imlib2.git"
#	EGIT_REPO_URI="file:///build/git-mirror/imlib2.git"
#	EGIT_BRANCH=master

else
	SRC_URI="https://downloads.sourceforge.net/enlightenment/${P}.tar.xz"
fi


LICENSE="BSD"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~ia64 ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~sparc-solaris ~sparc64-solaris ~x64-solaris ~x86-solaris"
IUSE="+X apidoc bzip2 cpu_flags_x86_mmx cpu_flags_x86_sse2 debug
eps +gif +jpeg jpeg2k jpegxl heif lzma mp3 +png +shm static-libs
svg +tiff +webp zlib"

REQUIRED_USE="shm? ( X )"

RDEPEND="
	X? (
		x11-libs/libX11[${MULTILIB_USEDEP}]
		x11-libs/libXext[${MULTILIB_USEDEP}]
	)
	bzip2? ( app-arch/bzip2[${MULTILIB_USEDEP}] )
	eps? ( app-text/libspectre )
	gif? ( media-libs/giflib:=[${MULTILIB_USEDEP}] )
	heif? ( media-libs/libheif:=[${MULTILIB_USEDEP}] )
	jpeg2k? ( media-libs/openjpeg:=[${MULTILIB_USEDEP}] )
	jpeg? ( media-libs/libjpeg-turbo:=[${MULTILIB_USEDEP}] )
	jpegxl? ( media-libs/libjxl:=[${MULTILIB_USEDEP}] )
	lzma? ( app-arch/xz-utils[${MULTILIB_USEDEP}] )
	media-libs/freetype:2[${MULTILIB_USEDEP}]
	mp3? ( media-libs/libid3tag:=[${MULTILIB_USEDEP}] )
	png? ( >=media-libs/libpng-1.6.10:0=[${MULTILIB_USEDEP}] )
	svg? ( >=gnome-base/librsvg-2.46.0:*[${MULTILIB_USEDEP}] )
	tiff? ( >=media-libs/tiff-4.0.4:=[${MULTILIB_USEDEP}] )
	webp? ( media-libs/libwebp:=[${MULTILIB_USEDEP}] )
	zlib? ( sys-libs/zlib[${MULTILIB_USEDEP}] )
	!<media-plugins/imlib2_loaders-1.7.0
"
DEPEND="${RDEPEND}
	X? ( x11-base/xorg-proto )"
BDEPEND="
	virtual/pkgconfig
	apidoc? ( app-doc/doxygen )
"

# default DOCS will haul README.in we do not need
# ;madhu 230109 README is not available in git.
DOCS=( AUTHORS ChangeLog TODO)


src_prepare()
{
	default
	if ${USE_GIT}; then
		eautoreconf
	fi
}

multilib_src_configure() {
	local myeconfargs=(
		$(use_with X x)
		$(multilib_native_use_enable apidoc doc-build)
		$(use_with bzip2 bz2)
		$(use_enable debug)
		$(multilib_native_use_with eps ps)
		$(use_with gif)
		$(use_with heif)
		$(use_with jpeg)
		$(use_with jpeg2k j2k)
		$(use_with jpegxl jxl)
		$(use_with lzma)
		$(use_with mp3 id3)
		$(use_with png)
		$(use_with shm x-shm-fd)
		$(use_enable static-libs static)
		$(use_with svg)
		$(use_with tiff)
		$(use_with webp)
		$(use_with zlib)
	)

	# imlib2 has different configure options for x86/amd64 assembly
	if [[ $(tc-arch) == amd64 ]]; then
		myeconfargs+=( $(use_enable cpu_flags_x86_sse2 amd64) --disable-mmx )
	else
		myeconfargs+=( --disable-amd64 $(use_enable cpu_flags_x86_mmx mmx) )
	fi

	ECONF_SOURCE="${S}" \
	econf "${myeconfargs[@]}"
}

multilib_src_install() {
	V=1 emake install DESTDIR="${D}"
	find "${D}" -name '*.la' -delete || die

	multilib_is_native_abi && export DOCS+=( ${BUILD_DIR}/README )

	multilib_is_native_abi && use apidoc &&
		export HTML_DOCS=( "${BUILD_DIR}/doc/html/"* )
}

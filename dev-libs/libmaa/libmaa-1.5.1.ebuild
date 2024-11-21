# Copyright 2023-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Nov 21 15:23:36 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241121 1.4.7 -> 1.5.1, fix docs
# curl -4 --head 'http://excellmedia.dl.sourceforge.net/project/dict/libmaa/libmaa-1.5.1/libmaa-1.5.1.tar.gz'

EAPI=8

inherit multiprocessing toolchain-funcs

DESCRIPTION="Library with low-level data structures which are helpful for writing compilers"
HOMEPAGE="https://dict.org/"
SRC_URI="https://downloads.sourceforge.net/dict/${P}.tar.gz"

LICENSE="MIT"
SLOT="0/4"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86"

BDEPEND="dev-util/mk-configure"

PATCHES=(
	"${FILESDIR}"/${PN}-1.4.7-makefile-respect-flags.patch
)

src_configure() {
	local jobs="$(makeopts_jobs)"
	unset MAKEOPTS

	export MAKEOPTS="-j${jobs}"
	export MAKE=bmake

	MAKEARGS=(
		AR="$(tc-getAR)"
		CC="$(tc-getCC)"
		CXX="$(tc-getCXX)"
		NM="$(tc-getNM)"
		OBJCOPY="$(tc-getOBJCOPY)"
		OBJDUMP="$(tc-getOBJDUMP)"
		RANLIB="$(tc-getRANLIB)"
		STRIP="$(tc-getSTRIP)"
		#SIZE="$(tc-getSIZE)"

		# Don't use LD, use the compiler driver instead
		LDCOMPILER=yes

		CFLAGS="${CFLAGS}"
		CXXFLAGS="${CXXFLAGS}"
		LDFLAGS="${LDFLAGS}"

		# Our toolchain already handles these
		MKPIE=no
		USE_SSP=no
		USE_RELRO=no
		USE_FORT=no

		# No -Werror
		WARNERR=no

		INSTALL="${INSTALL:-${BROOT}/usr/bin/install}"

		# Don't calcify compiler settings in installed files
		MKCOMPILERSETTINGS=yes

		PREFIX="${EPREFIX}/usr"
		DOCDIR="${EPREFIX}/usr/share/doc/${PF}"
		INFODIR="${EPREFIX}/usr/share/info"
		LIBDIR="${EPREFIX}/usr/$(get_libdir)"
		MANDIR="${EPREFIX}/usr/share/man"

		MKFILESDIR="${BROOT}/usr/share/mk-configure/mk"
		BUILTINSDIR="${BROOT}/usr/share/mk-configure/builtins"
		FEATURESDIR="${BROOT}/usr/share/mk-configure/feature"
	)

#;madhu 241121 mk-configure 0.39.0+ wants some of these variables to be
#	export USE_CC="$(tc-getCC)"
#	export USE_CXX="$(tc-getCXX)"
#	export USE_NM="$(tc-getNM)"
#	export USE_INSTALL="${INSTALL:-${BROOT}/usr/bin/install}"
#	export USE_CC_COMPILERS=${USE_CC}
#	export USE_CXX_COMPILERS=${USE_CXX}

	mkcmake "${MAKEARGS[@]}" -j1 configure || die
}

src_compile() {
	mkcmake "${MAKEARGS[@]}" all || die
}

src_test() {
	mkcmake "${MAKEARGS[@]}" test || die
}

src_install() {
	mkcmake "${MAKEARGS[@]}" DESTDIR="${ED}" install

#;madhu 241121 install doesnt install docs
#	rm "${ED}"/usr/share/doc/${PF}/LICENSE || die

	dodoc README doc/NEWS doc/TODO
	dodoc doc/libmaa.600dpi.ps
	docompress -x /usr/share/doc/${PF}/libmaa.600dpi.ps

	# don't want static or libtool archives, #401935
	find "${D}" \( -name '*.a' -o -name '*.la' \) -delete || die
}

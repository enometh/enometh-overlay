# Copyright 2023-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Nov 21 15:36:01 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241121 0.39.3 -> 0.40.0. had to export USE_CC_COMPILER in src_compile before bmake  could find the compiler in the compiler phase. works ok outside portage.
# ?viasfs=1 curl -4 --head 'http://excellmedia.dl.sourceforge.net/project/mk-configure/mk-configure/mk-configure-0.40.0/mk-configure-0.40.0.tar.gz',

EAPI=8

inherit multiprocessing toolchain-funcs

DESCRIPTION="Lightweight replacement for GNU autotools"
HOMEPAGE="https://sourceforge.net/projects/mk-configure/"
SRC_URI="https://downloads.sourceforge.net/${PN}/${PN}/${P}.tar.gz"

LICENSE="BSD BSD-2 GPL-2+ MIT"
SLOT="0"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86"

# TODO: investigate
RESTRICT="test"

RDEPEND="
	|| ( x11-misc/makedepend dev-build/pmake )
	dev-build/bmake
"
BDEPEND="${RDEPEND}"

src_configure() {
	local jobs="$(makeopts_jobs)"
	unset MAKEOPTS

	export MAKEOPTS="-j${jobs}"
	export MAKEOPTS=-j1
	export MAKE=bmake
}

src_compile() {
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
		MKPIE=nod
		USE_SSP=no
		USE_RELRO=no
		USE_FORT=no

		# No -Werror
		WARNERR=no

		INSTALL="${INSTALL:-${BROOT}/usr/bin/install}"

		# Don't calcify compiler settings in installed files
		MKCOMPILERSETTINGS=force

		PREFIX="${EPREFIX}/usr"
		DOCDIR="${EPREFIX}/usr/share/doc/${PF}"
		INFODIR="${EPREFIX}/usr/share/info"
		LIBDIR="${EPREFIX}/usr/$(get_libdir)"
		MANDIR="${EPREFIX}/usr/share/man"

		MKFILESDIR="${BROOT}/usr/share/mk-configure/mk"
		BUILTINSDIR="${BROOT}/usr/share/mk-configure/builtins"
		FEATURESDIR="${BROOT}/usr/share/mk-configure/feature"
	)

#;madhu 241121 - fix strange problem with portage-3.0.66 and
# mk-configure-0.40.0 CC gets unset at this point and bmake mk doesn't
# find any C compiler or use its MAKEARGS. debug with bmake -dxcljecC |& tee
# /dev/shm/portage/bmake.out

	export USE_CC="$(tc-getCC)"
	export USE_CXX="$(tc-getCXX)"
	export USE_NM="$(tc-getNM)"
	export USE_INSTALL="${INSTALL:-${BROOT}/usr/bin/install}"
#	export USE_ID=/usr/bin/id
	export USE_CC_COMPILERS=${USE_CC}
	export USE_CXX_COMPILERS=${USE_CXX}
	emake cleandir-presentation "${MAKEARGS[@]}"
	emake "${MAKEARGS[@]}"
}

src_test() {
	emake "${MAKEARGS[@]}" test
}

src_install() {
	emake "${MAKEARGS[@]}" DESTDIR="${ED}" install

	rm "${ED}"/usr/share/doc/${PF}/LICENSE || die
}

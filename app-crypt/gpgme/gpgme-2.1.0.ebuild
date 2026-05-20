# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed May 20 11:34:50 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260520 2.1.0, drop gentoo cruft: alternatives, verify-sig, bogus IUSE+=cxx for keeping old packages, but provided by gpgmepp

EAPI=8

inherit libtool flag-o-matic out-of-source

DESCRIPTION="GnuPG Made Easy is a library for making GnuPG easier to use"
HOMEPAGE="https://www.gnupg.org/related_software/gpgme"
SRC_URI="
	mirror://gnupg/gpgme/${P}.tar.bz2
"

LICENSE="GPL-2 LGPL-2.1"
# Please check ABI on each bump, even if SONAMEs didn't change: bug #833355
# Subslot: SONAME of each: <libgpgme.FUDGE>
SLOT="1/45.0"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 ~sparc x86 ~arm64-macos ~x64-macos ~x64-solaris"
IUSE="common-lisp static-libs test cxx"
RESTRICT="!test? ( test )"

RDEPEND="
	>=app-crypt/gnupg-2
	>=dev-libs/libassuan-2.5.3:=
	>=dev-libs/libgpg-error-1.46-r1:=
"
DEPEND="${RDEPEND}"
#doc? ( app-text/doxygen[dot] )

src_prepare() {
	default

	elibtoolize

	# bug #697456
	addpredict /run/user/$(id -u)/gnupg

	local MAX_WORKDIR=66
	if use test && [[ "${#WORKDIR}" -gt "${MAX_WORKDIR}" ]]; then
		eerror "Unable to run tests as WORKDIR='${WORKDIR}' is longer than ${MAX_WORKDIR} which causes failure!"
		die "Could not run tests as requested with too-long WORKDIR."
	fi

	# Make best effort to allow longer PORTAGE_TMPDIR as usock limitation
	# fails build/tests.
	ln -s "${P}" "${WORKDIR}/b" || die
	S="${WORKDIR}/b"
}

my_src_configure() {
	# bug #847955
	append-lfs-flags

	local languages=(
		$(usev common-lisp 'cl')
	)

	local myeconfargs=(
		$(use test || echo "--disable-gpgconf-test --disable-gpg-test --disable-gpgsm-test --disable-g13-test")
		--enable-languages="${languages[*]}"
		$(use_enable static-libs static)
		GPGRT_CONFIG="${ESYSROOT}/usr/bin/${CHOST}-gpgrt-config"
	)

	ECONF_SOURCE="${S}" econf "${myeconfargs[@]}"
}

my_src_install() {
	emake DESTDIR="${D}" install
	find "${ED}" -type f -name '*.la' -delete || die
}

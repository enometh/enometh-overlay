# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Nov 06 17:02:30 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 261106 - 20261022 - drop optinal signature, perl threads, devel-size

EAPI=8

DESCRIPTION="Shell tool for executing jobs in parallel locally or on remote machines"
HOMEPAGE="https://www.gnu.org/software/parallel/ https://git.savannah.gnu.org/cgit/parallel.git/"
SRC_URI="mirror://gnu/${PN}/${P}.tar.bz2"

LICENSE="GPL-3+"
SLOT="0"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~loong ~mips ~ppc ~ppc64 ~riscv ~x86 ~x64-macos"

RDEPEND="
	dev-lang/perl
	dev-perl/Text-CSV
"
DEPEND="${RDEPEND}"

src_configure() {
	# bug #908214
	unset PARALLEL_HOME

	econf --docdir="${EPREFIX}"/usr/share/doc/${PF}/html
}

src_compile() {
	# Silence a warning where it tries to use pod2pdf; force it to fallback
	# to pre-generated PDF.
	mkdir "${T}"/fake || die
	ln -s "${BROOT}"/bin/false "${T}"/fake/pod2pdf || die
	export PATH="${T}/fake:${PATH}"

	default
}

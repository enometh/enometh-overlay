# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Oct 21 14:21:50 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241021 2.0.1-r2 -> 5.3.0, IUSE drop tcl,socks5 add ruby,python,sanitize

EAPI=8

DESCRIPTION="Epic5 IRC Client"
SRC_URI="ftp://ftp.epicsol.org/pub/epic/EPIC5-PRODUCTION/${P}.tar.xz"
HOMEPAGE="http://epicsol.org/"

PYTHON_COMPAT=( python3_9 python3_{10..13} )

RUBY_OPTIONAL="yes"
USE_RUBY="ruby26 ruby31 ruby27" # ;madhu 241021 probably needs eclass patching

inherit ruby-ng python-single-r1

LICENSE="BSD"
SLOT="0"
KEYWORDS="amd64 ~ppc ~riscv x86"

IUSE="archive perl valgrind ipv6 ruby python sanitize"

REQUIRED_USE="
 python? ( ${PYTHON_REQUIRED_USE} )
 ruby? ( || ( $(ruby_get_use_targets) ) )
"

RDEPEND="
	>=dev-libs/openssl-0.9.8e-r3:0=
	>=sys-libs/ncurses-5.6-r2:0=
	virtual/libcrypt:=
	virtual/libiconv
	archive? ( app-arch/libarchive )
	perl? ( >=dev-lang/perl-5.8.8-r2:= )
	ruby? (  $(ruby_implementations_depend) )
	python? ( ${PYTHON_DEPS} )
"
DEPEND="${RDEPEND}
	valgrind? ( dev-debug/valgrind )
"

S="${WORKDIR}/${P}"
src_configure() {
	econf \
		--libexecdir="${EPREFIX}"/usr/lib/misc \
		$(use_with ipv6) \
		$(use_with ruby) \
		$(use_with archive libarchive) \
		$(use_with perl) \
		$(use_with valgrind) \
		$(use_with sanitize clang-sanitizing)
}

src_prepare() {
	# prevent ruby-ng.eclass from messing with src_unpack
	default
}

src_unpack() {
	# prevent ruby-ng.eclass from messing with src_unpack
	default
}

src_compile() {
	# parallel build failure
	emake -j1
}

src_install() {
	default

	dodoc BUG_FORM COPYRIGHT EPIC4-USERS-README README KNOWNBUGS VOTES

	cd "${S}"/doc || die
	docinto doc
	dodoc \
		*.txt colors EPIC* IRCII_VERSIONS missing \
		nicknames outputhelp README.SSL SILLINESS TS4
}

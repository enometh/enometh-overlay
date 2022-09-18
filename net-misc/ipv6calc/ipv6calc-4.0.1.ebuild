# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2022-09-18 11:54:13 IST>
#   Touched: Sun Sep 18 11:47:39 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220918 - fix IUSE=maxminddb for GeoIP2 support

EAPI="8"

DESCRIPTION="IPv6 address calculator"
HOMEPAGE="https://www.deepspace6.net/projects/ipv6calc.html"
SRC_URI="https://github.com/pbiering/${PN}/archive/${PV}.tar.gz -> ${P}.tar.gz"

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~alpha ~amd64 ~hppa ~ppc ~ppc64 ~sparc ~x86 ~amd64-linux ~x86-linux"
IUSE="cgi geoip maxminddb test"
RESTRICT="!test? ( test )"

RDEPEND="
	cgi? (
		dev-perl/URI
		dev-perl/Digest-SHA1
	)
	dev-libs/openssl:=
	geoip? ( >=dev-libs/geoip-1.4.7 )
	maxminddb? ( dev-libs/libmaxminddb:= )
"
DEPEND="${RDEPEND}
	test? ( dev-perl/Digest-SHA1 )
"

src_configure() {
	# These options are broken.  You can't disable them.  That's
	# okay because we want then force enabled.
	# --disable-db-as-registry
	# --disable-db-cc-registry
	local myeconfargs=(
		--disable-compiler-warning-to-error
		--disable-bundled-getopt
		--disable-bundled-md5
		--enable-shared
		--enable-dynamic-load
		--enable-db-ieee
		--enable-db-ipv4
		--enable-db-ipv6
		--disable-dbip
		--disable-dbip2
		--disable-external
		--disable-ip2location
		--enable-openssl-evp-md5
		--enable-openssl-md5
		$(use_enable maxminddb mmdb)
		$(use_with maxminddb mmdb-dynamic)
		$(use_enable geoip)
		$(use_enable cgi mod_ipv6calc )
	)

	if use geoip; then
		myeconfargs+=( "--with-geoip-db=${EPREFIX}/usr/share/GeoIP" )
	fi

	econf "${myeconfargs[@]}"
}

src_compile() {
	emake distclean
	# Disable default CFLAGS (-O2 and -g)
	emake DEFAULT_CFLAGS=""
}

src_test() {
	if [[ ${EUID} -eq 0 ]]; then
		# Disable tests that fail as root
		echo true > ipv6logstats/test_ipv6logstats.sh
	fi
	default
}

src_install() {
	emake DESTDIR="${D}" install
	dodoc ChangeLog CREDITS README TODO USAGE
}

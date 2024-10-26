# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Mar 25 18:43:47 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 240325 9.16.48 -> 9.19.22 drop patches, no caps,python (configure pulls it in, and dtrace),json-c, set python_single_target for sphinx, no html docs. builds static binaries.
#
# ;madhu 241026 9.21.2 --with-openssl gone,

EAPI=8
PYTHON_COMPAT=( python3_9 )
VERIFY_SIG_OPENPGP_KEY_PATH=/usr/share/openpgp-keys/isc.asc
inherit autotools flag-o-matic multiprocessing toolchain-funcs verify-sig python-any-r1

MY_PN=${PN//-tools}
MY_PV=${PV/_p/-P}
MY_PV=${MY_PV/_rc/rc}
MY_P="${MY_PN}-${MY_PV}"

DESCRIPTION="bind tools: dig, nslookup, host, nsupdate, dnssec-keygen"
HOMEPAGE="https://www.isc.org/software/bind https://gitlab.isc.org/isc-projects/bind9"
SRC_URI="
	https://downloads.isc.org/isc/bind9/${PV}/${MY_P}.tar.xz
	verify-sig? ( https://downloads.isc.org/isc/bind9/${PV}/${MY_P}.tar.xz.asc )
"
S="${WORKDIR}/${MY_P}"

LICENSE="Apache-2.0 BSD BSD-2 GPL-2 HPND ISC MPL-2.0"
SLOT="0"
KEYWORDS="amd64 x86"
IUSE="doc gssapi idn libedit readline test xml"
# no PKCS11 currently as it requires OpenSSL to be patched, also see bug #409687
RESTRICT="!test? ( test )"

# libuv lower bound should be the highest value seen at
# https://gitlab.isc.org/isc-projects/bind9/-/blob/bind-9.16/lib/isc/netmgr/netmgr.c?ref_type=heads#L244
# to avoid issues with matching stable/testing, etc
RDEPEND="
	>=dev-libs/libuv-1.42.0:=
	dev-libs/openssl:=
	xml? ( dev-libs/libxml2 )
	idn? ( net-dns/libidn2:= )
	gssapi? ( virtual/krb5 )
	libedit? ( dev-libs/libedit )
	!libedit? (
		readline? ( sys-libs/readline:= )
	)
"
DEPEND="${RDEPEND}"
# sphinx required for man-page and html creation
#	doc? ( dev-python/sphinx )
BDEPEND="
	virtual/pkgconfig
	dev-python/sphinx
	test? (
		dev-util/cmocka
		dev-util/kyua
	)
	verify-sig? ( sec-keys/openpgp-keys-isc )
"
RESTRICT="test"

src_prepare() {
	default
}

src_configure() {
	local myeconfargs=(
		# localstatedir for nsupdate -l, bug #395785
		--localstatedir="${EPREFIX}"/var
		#configure.ac:115 AC_MSG_ERROR([Static linking is not supported as it disables dlopen() and certain security features (e.g. RELRO, ASLR)]
		--enable-static --disable-shared --enable-developer
		--without-json-c
		--without-zlib
		--without-lmdb
		--without-maxminddb
		--disable-geoip
#		--with-openssl="${ESYSROOT}"/usr
		--disable-dnstap
		$(use_with idn libidn2 "${ESYSROOT}"/usr)
		$(use_with xml libxml2)
		$(use_with gssapi)
		AR="$(type -P $(tc-getAR))"
	)

	# bug #607400
	if use libedit ; then
		myeconfargs+=( --with-readline=libedit )
	elif use readline ; then
		myeconfargs+=( --with-readline=readline )
	else
		myeconfargs+=( --without-readline )
	fi

	append-ldflags "-L${ESYSROOT}/usr/$(get_libdir)"
	tc-export BUILD_CC
	econf "${myeconfargs[@]}"
}

src_compile() {
	local AR="$(tc-getAR)"

	emake AR="${AR}" -C lib/
	emake AR="${AR}" bind.keys.h
	emake AR="${AR}" -C bin/delv/
	emake AR="${AR}" -C bin/dig/
	emake AR="${AR}" -C bin/nsupdate/
	emake AR="${AR}" -C bin/dnssec/
	#;madhu 240325 make doc is a noop. make man requires sphinx anyway
	emake -C doc/man/ man #$(usev doc)
}

src_test() {
	# system tests ('emake test') require network configuration for IPs etc
	# so we run the unit tests instead.
	TEST_PARALLEL_JOBS="$(makeopts_jobs)" emake -Onone unit
}

src_install() {
	local man_dir="${S}/doc/man"
	local html_dir="${man_dir}/_build/html"

	dodoc README.md CHANGES OPTIONS.md
	for i in notes dnssec-guide misc; do
		dodoc -r doc/$i
	done

	# any better way to do this?
	for i in notes dnssec-guide misc; do
		docompress -x /usr/share/doc/${PF}/$i
	done

	#;madhu 240325 - have to get the exe files from .libs, man files are
	# under man(1)

	cd "${S}"/bin/delv || die
	dobin delv
	doman ${man_dir}/delv.1

	cd "${S}"/bin/dig || die
	dobin dig host nslookup
	doman ${man_dir}/{dig,host,nslookup}.1

	cd "${S}"/bin/nsupdate || die
	dobin nsupdate
	doman ${man_dir}/nsupdate.1

	cd "${S}"/bin/dnssec || die
	local tool
	for tool in dsfromkey importkey keyfromlabel keygen \
		revoke settime signzone verify; do
		dobin dnssec-"${tool}"
		doman ${man_dir}/dnssec-"${tool}".1
	done

}

# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Jun 14 18:05:22 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200614 4.0.ebuild->git
# ;madhu 210303 4.2
# ;madhu 230709 5.6-r2

#MY_NTOPNG_COMMIT=b56a0ef5be5adc1dc9fe244a89e0214ae9d96fb0 + patches
#MY_NTOPNG_DIST_COMMIT=11001fe95dc1e8ddd01801edfc75c0170df0cceb

EAPI=8

inherit autotools toolchain-funcs

USE_GIT=true

if ${USE_GIT}; then
	inherit git-r3
	SRC_URI=""
	EGIT_REPO_URI="https://github.com/ntop/ntopng/"
#	EGIT_REPO_URI="file:///build/git-mirror/ntopng.git"
	EGIT_BRANCH="madhu-dev"
	EGIT_CLONE_TYPE="shallow"
	EGIT_SUBMODULES=(
		#tests/e2e
		dist
	)
else
# Check this on bumps, get latest commit from the relevant branch (e.g. 5.6-stable)
# See bug #894152 and https://github.com/ntop/ntopng/issues/7203
NTOPNG_DIST_COMMIT="90d81ad0281eb6eb582a683ac321a3959abb1269"
SRC_URI="https://github.com/ntop/ntopng/archive/${PV}.tar.gz -> ${P}.tar.gz"
SRC_URI+=" https://github.com/ntop/ntopng-dist/archive/${NTOPNG_DIST_COMMIT}.tar.gz -> ${P}-web-${NTOPNG_DIST_COMMIT}.tar.gz"
fi

DESCRIPTION="Network traffic analyzer with web interface"
HOMEPAGE="https://www.ntop.org/"

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64 ~x86"

DEPEND="dev-db/mysql-connector-c:=
	dev-db/sqlite:3
	dev-libs/hiredis:=
	dev-libs/json-c:=
	dev-libs/libmaxminddb
	dev-libs/libsodium:=
	dev-libs/openssl:=
	net-analyzer/rrdtool
	net-libs/libpcap
	>=net-libs/nDPI-4.7:=
	<net-libs/nDPI-4.8:=
	>=net-libs/zeromq-3:=
	net-misc/curl
	sys-libs/libcap
	sys-libs/zlib"

RDEPEND="${DEPEND}
	acct-user/ntopng
	acct-group/ntopng
	dev-db/redis"

if ! ${USE_GIT} ; then
die ";madhu 230709 not updated"
PATCHES=(
#	"${FILESDIR}"/${PN}-5.2.1-mysqltool.patch
#	"${FILESDIR}"/${PN}-5.4-ndpi-linking.patch
)
fi

BDEPEND="virtual/pkgconfig"

RESTRICT="test"

src_prepare() {
	default

	# Follows upstream's autogen.sh
	sed \
		-e "s/@VERSION@/${PV}.$(date +%y%m%d)/g" \
		-e "s/@SHORT_VERSION@/${PV}/g" \
		-e "s/@GIT_DATE@/$(date)/g" \
		-e "s/@GIT_RELEASE@/${PV}.$(date +%y%m%d)/g" \
		-e "s/@GIT_BRANCH@//g" < "${S}/configure.ac.in" \
		> "${S}/configure.ac" || die

	eautoreconf
}

src_configure() {
	tc-export PKG_CONFIG

	# configure.ac.in at least has some bashisms(?) which get lost(?)
	# in conversion to configure.ac (like [ -> nothing?) so just force
	# bash for now. It's still not quite right but at least upstream will be
	# testing with it. TODO: fix this!
	CONFIG_SHELL="${BROOT}/bin/bash" econf --with-ndpi-includes="${ESYSROOT}"/usr/include/ndpi
}

src_compile() {
	emake \
		CC="$(tc-getCC)" \
		MYCFLAGS="${CFLAGS}" \
		MYLDFLAGS="${LDFLAGS}"
}

src_install() {
	SHARE_NTOPNG_DIR="${EPREFIX}/usr/share/${PN}"
	insinto "${SHARE_NTOPNG_DIR}"
	doins -r httpdocs
	doins -r scripts

	if ${USE_GIT}; then
		find ${ED}/${SHARE_NTOPNG_DIR} -name "*~"   | xargs /bin/rm -fv
		find ${ED}/${SHARE_NTOPNG_DIR} -name ".git" | xargs /bin/rm -rfv
	else
	insinto "${SHARE_NTOPNG_DIR}"/httpdocs/dist
	doins -r "${WORKDIR}"/ntopng-dist-${NTOPNG_DIST_COMMIT}/.
	fi

	insinto "${SHARE_NTOPNG_DIR}/third-party"
	doins -r third-party/i18n.lua-master
	doins -r third-party/lua-resty-template-master

	exeinto /usr/bin
	doexe "${PN}"
	doman "${PN}.8"

	newinitd "${FILESDIR}"/ntopng.init.d ntopng
	newconfd "${FILESDIR}"/ntopng.conf.d ntopng

	keepdir /var/lib/ntopng
	fowners ntopng /var/lib/ntopng
}

pkg_postinst() {
	elog "ntopng default credentials are user='admin' password='admin'"
}

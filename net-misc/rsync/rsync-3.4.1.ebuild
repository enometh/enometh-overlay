# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu May 09 07:58:26 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190126 bump up 3.1.3 to 3.1.3-r1 and apply user patches
# ;madhu 200927 v3.2.3-28-g740ed11a EGIT_OVERRIDE_BRANCH_WAYNED_RSYNC=madhu-tip
# md5p8.diff omit-dir-changes.diff direct-io.diff downdate.diff clone-dest.diff  backup-deleted.diff copy-devices.diff
#
# ;madhu 230818 3.3.0-pre1 (based on 3.7.2-r2) -undo retarding on ipv6 rrsync (todo). git-r3.eclass cannot handle twouri's (to fetch patches from say git://git.samba.org/rsync-patches.git), ship patches in filesdir.
#
# ;madhu 250307 3.4.1 drop verify-sig, rsync-patches adapted from
# https://download.samba.org/pub/rsync/src/rsync-patches-3.4.1.tar.gz

EAPI=8
USE_GIT=false

# MY_COMMIT=" 9994933c8ccf7ead27c81fe4ce2eb4e08af20c7f" # v3.4.1-8-g9994933c

# Uncomment when introducing a patch which touches configure
RSYNC_NEEDS_AUTOCONF=1
PYTHON_COMPAT=( python3_{10..13} )
inherit flag-o-matic prefix python-single-r1 systemd

DESCRIPTION="File transfer program to keep remote files into sync"
HOMEPAGE="https://rsync.samba.org/"
if [[ ${PV} == *9999 ]] || ${USE_GIT} ; then
	EGIT_REPO_URI="https://github.com/WayneD/rsync.git"
	inherit autotools git-r3
	EGIT_BRANCH=master
	SRC_URI=""
#	EGIT_CLONE_TYPE=shallow
#	EGIT_COMMIT=$MY_COMMIT

	REQUIRED_USE="${PYTHON_REQUIRED_USE}"
else

	if [[ -n ${RSYNC_NEEDS_AUTOCONF} ]] ; then
		inherit autotools
	fi

	if [[ ${PV} == *_pre* ]] ; then
		SRC_DIR="src-previews"
	else
		SRC_DIR="src"
	fi

	SRC_URI="https://rsync.samba.org/ftp/rsync/${SRC_DIR}/${P/_/}.tar.gz"
	S="${WORKDIR}"/${P/_/}
fi
		KEYWORDS="~alpha amd64 arm arm64 hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"

LICENSE="GPL-3"
SLOT="0"
IUSE="acl examples iconv lz4 rrsync ssl stunnel system-zlib xattr xxhash zstd"
REQUIRED_USE+=" examples? ( ${PYTHON_REQUIRED_USE} )"
REQUIRED_USE+=" rrsync? ( ${PYTHON_REQUIRED_USE} )"

RDEPEND="
	>=dev-libs/popt-1.5
	acl? ( virtual/acl )
	examples? (
		${PYTHON_DEPS}
		dev-lang/perl
	)
	lz4? ( app-arch/lz4:= )
	rrsync? (
		${PYTHON_DEPS}
		$(python_gen_cond_dep '
			dev-python/bracex[${PYTHON_USEDEP}]
		')
	)
	ssl? ( dev-libs/openssl:= )
	system-zlib? ( sys-libs/zlib )
	xattr? ( kernel_linux? ( sys-apps/attr ) )
	xxhash? ( >=dev-libs/xxhash-0.8 )
	zstd? ( >=app-arch/zstd-1.4:= )
	iconv? ( virtual/libiconv )"
DEPEND="${RDEPEND}"
BDEPEND="
	examples? ( ${PYTHON_DEPS} )
	rrsync? ( ${PYTHON_DEPS} )
"

if [[ ${PV} == *9999 ]] || ${USE_GIT}; then
	BDEPEND+=" ${PYTHON_DEPS}
		$(python_gen_cond_dep '
			dev-python/commonmark[${PYTHON_USEDEP}]
		')"
else
	:
fi

PATCHES=(
	# Temporary just for the bug #948106 CVE fixes
	"${FILESDIR}"/3.4.1
)

pkg_setup() {
	# - USE=examples needs Python itself at runtime, but nothing else
	# - 9999 needs commonmark at build time
	if  [[ ${PV} == *9999 ]] || ${USE_GIT} || use examples || use rrsync; then
		python-single-r1_pkg_setup
	fi
}

src_prepare() {
	default

	sed -i -e 's/AC_HEADER_MAJOR_FIXED/AC_HEADER_MAJOR/' configure.ac

	if [[ ${PV} == *9999 || -n ${RSYNC_NEEDS_AUTOCONF} ]] ||  ${USE_GIT} ; then
		eaclocal -I m4
		eautoconf -o configure.sh
		eautoheader && touch config.h.in
	fi

	if use examples || use rrsync; then
		python_fix_shebang support/
	fi

	if [[ -f rrsync.1 ]]; then
		# If the pre-build rrsync.1 man page exists, then link to it
		# from support/rrsync.1 to avoid rsync's build system attempting
		# re-creating the man page (bug #883049).
		ln -s ../rrsync.1 support/rrsync.1 || die
	fi
}

src_configure() {
	# Should be fixed upstream in next release (>3.3.0) (bug #943745)
	append-cflags $(test-flags-CC -std=gnu17)

	local myeconfargs=(
		--with-rsyncd-conf="${EPREFIX}"/etc/rsyncd.conf
		--without-included-popt
		--enable-ipv6
		$(use_enable acl acl-support)
		$(use_enable iconv)
		$(use_enable lz4)
		$(use_with rrsync)
		$(use_enable ssl openssl)
		$(use_with !system-zlib included-zlib)
		$(use_enable xattr xattr-support)
		$(use_enable xxhash)
		$(use_enable zstd)
	)

	# https://github.com/WayneD/rsync/pull/428
	if is-flagq -fsanitize=undefined ; then
		sed -E -i \
			-e 's:#define CAREFUL_ALIGNMENT (0|1):#define CAREFUL_ALIGNMENT 1:' \
			byteorder.h || die
		append-flags -DCAREFUL_ALIGNMENT
	fi

	econf "${myeconfargs[@]}"
}

src_install() {
	emake DESTDIR="${D}" install

	newconfd "${FILESDIR}"/rsyncd.conf.d rsyncd
	newinitd "${FILESDIR}"/rsyncd.init.d-r1 rsyncd

	dodoc NEWS.md README.md TODO tech_report.tex

	insinto /etc
	newins "${FILESDIR}"/rsyncd.conf-3.0.9-r1 rsyncd.conf

	insinto /etc/logrotate.d
	newins "${FILESDIR}"/rsyncd.logrotate rsyncd

	insinto /etc/xinetd.d
	newins "${FILESDIR}"/rsyncd.xinetd-3.0.9-r1 rsyncd

	# Install stunnel helpers
	if use stunnel ; then
		emake DESTDIR="${D}" install-ssl-daemon
	fi

	# Install the useful contrib scripts
	if use examples ; then
		# The 'rrsync' script is installed conditionally via the 'rrysnc'
		# USE flag, and not via the 'examples' USE flag.
		rm support/rrsync* || die

		exeinto /usr/share/rsync
		doexe support/*

		rm -f "${ED}"/usr/share/rsync/{Makefile*,*.c}
	fi

	eprefixify "${ED}"/etc/{,xinetd.d}/rsyncd*

	systemd_newunit packaging/systemd/rsync.service rsyncd.service
}

pkg_postinst() {
	if grep -Eqis '^[[:space:]]use chroot[[:space:]]*=[[:space:]]*(no|0|false)' \
		"${EROOT}"/etc/rsyncd.conf "${EROOT}"/etc/rsync/rsyncd.conf ; then
		ewarn "You have disabled chroot support in your rsyncd.conf.  This"
		ewarn "is a security risk which you should fix.  Please check your"
		ewarn "/etc/rsyncd.conf file and fix the setting 'use chroot'."
	fi

	if use stunnel ; then
		einfo "Please install \">=net-misc/stunnel-4\" in order to use stunnel feature."
		einfo
		einfo "You maybe have to update the certificates configured in"
		einfo "${EROOT}/etc/stunnel/rsync.conf"
	fi

	if use system-zlib ; then
		ewarn "Using system-zlib is incompatible with <rsync-3.1.1 when"
		ewarn "using the --compress option."
		ewarn
		ewarn "When syncing with >=rsync-3.1.1 built with bundled zlib,"
		ewarn "and the --compress option, add --new-compress (-zz)."
		ewarn
		ewarn "For syncing the portage tree, add:"
		ewarn "PORTAGE_RSYNC_EXTRA_OPTS=\"--new-compress\" to make.conf"
	fi
}

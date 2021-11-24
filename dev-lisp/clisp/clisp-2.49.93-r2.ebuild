# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2021-12-03 02:52:08 IST>
#   Touched: Sun Sep 01 04:14:35 2019 -0600 <madhu@cs.unm.edu>
#   Bugs-To: madhu@cs.unm.edu
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2021 Madhu.  All Rights Reserved.
#
# ;madhu 190901 2.49.93-r1 clisp from gitlab.com/gnu-clisp/clisp.git - no asdf
# ;madhu 211124 2.39.93-r2 USE_GIT
# ;madhu 211203 EAPI 8

EAPI=8

inherit flag-o-matic multilib toolchain-funcs xdg-utils

MY_COMMIT="de01f0f47bb44d3a0f9e842464cf2520b238f356"
USE_GIT=true

DESCRIPTION="A portable, bytecode-compiled implementation of Common Lisp"
HOMEPAGE="https://clisp.sourceforge.io/"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://gitlab.com/gnu-clisp/clisp.git/"
	EGIT_BRANCH="master"
	EGIT_COMMIT="${MY_COMMIT}"
else
	SRC_URI="mirror://gentoo/${P}.tar.bz2"
fi

# *   EGIT_OVERRIDE_REPO_GNU_CLISP_CLISP
# *   EGIT_OVERRIDE_BRANCH_GNU_CLISP_CLISP
# *   EGIT_OVERRIDE_COMMIT_GNU_CLISP_CLISP
# *   EGIT_OVERRIDE_COMMIT_DATE_GNU_CLISP_CLISP

LICENSE="GPL-2"
SLOT="2/8"
KEYWORDS="~alpha amd64 ~ia64 ppc ~sparc x86"
IUSE="hyperspec X berkdb dbus fastcgi gdbm gtk +pcre postgres +readline svm -threads +unicode +zlib"
# "jit" disabled ATM

RDEPEND="virtual/libcrypt:=
		 virtual/libiconv
		 >=dev-libs/libsigsegv-2.10
		 >=dev-libs/ffcall-1.10
		 dbus? ( sys-apps/dbus )
		 fastcgi? ( dev-libs/fcgi )
		 gdbm? ( sys-libs/gdbm:0= )
		 gtk? ( >=x11-libs/gtk+-2.10:2 >=gnome-base/libglade-2.6 )
		 postgres? ( >=dev-db/postgresql-8.0:* )
		 readline? ( >=sys-libs/readline-7.0:0= )
		 pcre? ( dev-libs/libpcre:3 )
		 svm? ( sci-libs/libsvm )
		 zlib? ( sys-libs/zlib )
		 X? ( x11-libs/libXpm )
		 hyperspec? ( dev-lisp/hyperspec )
		 berkdb? ( sys-libs/db:4.8 )"

DEPEND="${RDEPEND}
	X? ( x11-base/xorg-proto x11-misc/imake )"

enable_modules() {
	[[ $# = 0 ]] && die "${FUNCNAME[0]} must receive at least one argument"
	for m in "$@" ; do
		einfo "enabling module $m"
		myconf+=" --with-module=${m}"
	done
}

BUILDDIR="builddir"

# modules not enabled:
#  * berkdb: must figure out a way to make the configure script pick up the
#            currect version of the library and headers
#  * dirkey: fails to compile, requiring windows.h, possibly wrong #ifdefs
#  * matlab, netica: not in portage
#  * oracle: can't install oracle-instantclient


PATCHES=(
	$FILESDIR/clisp-2.49.93-r2-charstrg.d-name_string-allow-u-000c-syntax-with-.patch
	$FILESDIR/clisp-2.49.93-r2-record-source-locations.patch.patch
)

src_prepare() {
	# More than -O1 breaks alpha/ia64
	if use alpha || use ia64; then
		sed -i -e 's/-O2//g' src/makemake.in || die
	fi
	default
	xdg_environment_reset
}

src_configure() {
	# We need this to build on alpha/ia64
	if use alpha || use ia64; then
		replace-flags -O? -O1
	fi

	if use x86; then
		append-flags -falign-functions=4
	fi

	# built-in features
	local myconf="--with-ffcall --without-dynamic-modules"
#    There's a problem with jit_allocai function
#    if use jit; then
#        myconf+=" --with-jitc=lightning"
#    fi
	if use threads; then
		myconf+=" --with-threads=POSIX_THREADS"
	fi

	# default modules
	enable_modules rawsock
	# optional modules
	use elibc_glibc && enable_modules bindings/glibc
	use X && enable_modules clx/new-clx
	if use postgres; then
		enable_modules postgresql
		append-cppflags -I$(pg_config --includedir)
	fi
	if use berkdb; then
		enable_modules berkeley-db
		append-cppflags -I"${EPREFIX}"/usr/include/db4.8
	fi
	use dbus && enable_modules dbus
	use fastcgi && enable_modules fastcgi
	use gdbm && enable_modules gdbm
	use gtk && enable_modules gtk2
	use pcre && enable_modules pcre
	use svm && enable_modules libsvm
	use zlib && enable_modules zlib

	if use hyperspec; then
		CLHSROOT="file:///${EPREFIX}/usr/share/doc/hyperspec/HyperSpec/"
	else
		CLHSROOT="http://www.lispworks.com/reference/HyperSpec/"
	fi

	# configure chokes on --sysconfdir option
	local configure="./configure --prefix=${EPREFIX}/usr --enable-portability \
		  --libdir=${EPREFIX}/usr/$(get_libdir) $(use_with readline) $(use_with unicode) \
		  ${myconf} --hyperspec=${CLHSROOT} ${BUILDDIR}"
	einfo "${configure}"
	${configure} || die "./configure failed"

	IMPNOTES="file://${ROOT%/}/usr/share/doc/${PN}-${PVR}/html/impnotes.html"
	sed -i "s,http://clisp.cons.org/impnotes/,${IMPNOTES},g" \
		"${BUILDDIR}"/config.lisp || die "Cannot fix link to implementation notes"
}

src_compile() {
	export VARTEXFONTS="${T}"/fonts
	cd "${BUILDDIR}" || die
	# parallel build fails
	emake -j1
}

DOCS=( doc/{CLOS-guide,LISP-tutorial}.txt )
HTML_DOCS=( doc/impnotes.{css,html} doc/regexp.html doc/clisp.png )

src_install() {
	pushd "${BUILDDIR}"
	make DESTDIR="${D}" prefix="${EPREFIX}"/usr install-bin || die "Installation failed"
	doman clisp.1
	dodoc ../SUMMARY README* ../src/NEWS ../unix/MAGIC.add ../ANNOUNCE
	popd
	einstalldocs
}

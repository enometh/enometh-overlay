# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Feb 17 18:12:11 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2021 Madhu.  All Rights Reserved.
#
# ;madhu 190217 -16.1.3 build from git, no asdf
# ;madhu 190317 - convert to a 9999 build
# ;madhu 191125 - copy to 1.16.3-r2
# ;madhu 211121 - 21.2.1-r5 (USE_GIT) 21.2.1-115-g6aa02de4c

EAPI=7

USE_GIT=true

inherit readme.gentoo-r1

MY_P=ecl-${PV}
DESCRIPTION="ECL is an embeddable Common Lisp implementation"
HOMEPAGE="https://common-lisp.net/project/ecl/"

if [[ "${PV}" == 9999 ]] || ${USE_GIT} ; then
	#EGIT_REPO_URI="https://gitlab.com/embeddable-common-lisp/ecl.git"
	EGIT_REPO_URI="https://gitlab.common-lisp.net/embeddable-common-lisp/ecl.git"
	inherit git-r3

	EGIT_BRANCH="develop"
	EGIT_CLONE_TYPE="shallow"
#EGIT_MIRROR_URI="file:///build/git-mirror"
#EGIT_CHECKOUT_DIR="${WORKDIR}/${P}"
#EGIT_SUBMODULES=()
#S="${EGIT_CHECKOUT_DIR}"

else
SRC_URI="https://common-lisp.net/project/ecl/static/files/release/${MY_P}.tgz"
S="${WORKDIR}/${MY_P}"
fi

LICENSE="BSD-2 LGPL-2.1+"
SLOT="0/${PV}"
KEYWORDS="~amd64 ~ppc ~sparc ~x86 ~amd64-linux"
IUSE="cxx +debug emacs gengc precisegc cpu_flags_x86_sse +threads +unicode X"
# test phase only works if ecls already installed #516876
RESTRICT="test"

#CDEPEND		">=dev-lisp/asdf-2.33-r3:="
#DEPEND		app-text/texi2html

RDEPEND="dev-libs/gmp:0
		virtual/libffi:=
		dev-libs/libatomic_ops
		>=dev-libs/boehm-gc-7.1[threads?]"
DEPEND="${RDEPEND}
		app-text/texi2html
		emacs? ( >=app-editors/emacs-23.1:* >=app-eselect/eselect-emacs-1.12 )"

#S="${WORKDIR}"/${MY_P}
DOCS=( README.md CHANGELOG )

PATCHES=(
	"${FILESDIR}/${PN}-16.1.3-headers-gentoo.patch"
	"${FILESDIR}/${PN}-16.1.3-build.patch"
	"${FILESDIR}/${PN}-21.2.1-donotcompressinfo.patch"
	"${FILESDIR}/${PN}-21.2.1-extend-ffi-defcallback-to-support-exported-symbols.patch"
)

src_prepare() {
	default
#	cp "${EPREFIX}"/usr/share/common-lisp/source/asdf/build/asdf.lisp contrib/asdf/ || die
}

# 		$(use_with threads __thread) \
#		$(use_with unicode unicode-names) \
# 		--enable-longdouble=yes \
# 		--with-defsystem \

src_configure() {
	econf \
		--enable-gmp=system \
		--enable-boehm=system \
		--with-asdf=no \
		--with-defsystem=no \
		--with-ieee-fp=yes
		--with-dffi \
		$(use_with cxx) \
		$(use_enable gengc) \
		$(use_enable precisegc) \
		$(use_enable debug) \
		$(use_with debug debug-cflags) \
		--enable-libatomic=system \
		$(use_with cpu_flags_x86_sse sse) \
		$(use_enable threads) \
		$(use_enable unicode) \
		$(use_with X x)
}

src_compile() {
	if use emacs; then
		local ETAGS=$(eselect --brief etags list | sed -ne '/emacs/{p;q}')
		[[ -n ${ETAGS} ]] || die "No etags implementation found"
		pushd build > /dev/null || die
		emake ETAGS=${ETAGS} TAGS
		popd > /dev/null || die
	else
		touch build/TAGS
	fi

	emake
}

src_install () {
	default

	readme.gentoo_create_doc
	pushd build/doc || die
	newman ecl.man ecl.1
	newman ecl-config.man ecl-config.1
	popd || die
}

pkg_postinst() {
	readme.gentoo_print_elog
}

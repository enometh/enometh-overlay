# Copyright 1999-2024 Gentoo Authors
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
# ;madhu 240516 - 24.5.10  (USE_GIT) 24.5.10-248-gb32ccc3d8
# ;madhu 250805 - 25.5.10-r1 (USE_GIT) 24.5.10-441-gbebb43d
#  use bundled boehm-gc & libatomic_ops, but system gmp & system libffi, fix texinfo category

EAPI=8

USE_GIT=true
MY_COMMIT="bebb43d558e813f253523185e189ce831e4ad914"

inherit flag-o-matic readme.gentoo-r1

DESCRIPTION="ECL is an embeddable Common Lisp implementation"
HOMEPAGE="https://common-lisp.net/project/ecl/"

if [[ "${PV}" == 9999 ]] || ${USE_GIT} ; then
	EGIT_REPO_URI="https://gitlab.com/embeddable-common-lisp/ecl.git"
	EGIT_COMMIT="$MY_COMMIT"
#	EGIT_REPO_URI="https://gitlab.common-lisp.net/embeddable-common-lisp/ecl.git"
	inherit git-r3
	EGIT_BRANCH="develop"
	EGIT_CLONE_TYPE="shallow"
#EGIT_MIRROR_URI="file:///build/git-mirror"
#EGIT_CHECKOUT_DIR="${WORKDIR}/${P}"
#EGIT_SUBMODULES=()
#S="${EGIT_CHECKOUT_DIR}"

else
SRC_URI="https://common-lisp.net/project/ecl/static/files/release/${P}.tgz"
S="${WORKDIR}/${MY_P}"
fi

LICENSE="BSD-2 LGPL-2.1+"
SLOT="0/${PV}"
KEYWORDS="~amd64 ~ppc ~ppc64 ~riscv ~sparc ~x86 ~amd64-linux"
IUSE="cxx debug emacs gengc precisegc cpu_flags_x86_sse +threads +unicode X"
# test phase only works if ecl already installed #516876
RESTRICT="test"

#CDEPEND		">=dev-lisp/asdf-2.33-r3:="
#DEPEND		app-text/texi2html
#RDEPEND virtual/libffi:=

RDEPEND="dev-libs/gmp:0=
		dev-libs/libffi:=
		dev-libs/libatomic_ops
		>=dev-libs/boehm-gc-7.1[threads?]"
DEPEND="${RDEPEND}
		sys-apps/texinfo
		emacs? ( >=app-editors/emacs-23.1:* >=app-eselect/eselect-emacs-1.12 )"

DOCS=( README.md CHANGELOG )

PATCHES=(
	"${FILESDIR}/${PN}-16.1.3-headers-gentoo.patch"
	"${FILESDIR}/${PN}-16.1.3-build.patch"
	"${FILESDIR}/${PN}-21.2.1-donotcompressinfo.patch"
	# gcc15 in upstream
	"${FILESDIR}/${PN}-24.5.10-extend-ffi-defcallback-to-support-exported-symbols.patch"
)

src_prepare() {
	default
#	cp "${EPREFIX}"/usr/share/common-lisp/source/asdf/build/asdf.lisp contrib/asdf/ || die
}

# 		$(use_with threads __thread) \
#		$(use_with unicode unicode-names) \
# 		--enable-longdouble=yes \
# 		--with-defsystem \
#		--enable-libatomic=system \

src_configure() {
	append-cflags -std=gnu23
	filter-lto # bug #931081

	econf \
		--with-asdf=no \
		--with-defsystem=no \
		--enable-gmp=system \
		--enable-boehm=included \
		--with-dffi \
		--enable-libatomic=included \
		--with-ieee-fp=yes \
		--with-libffi-incdir=/usr/lib64/libffi/include \
		--with-libffi-libdir=/usr/lib64/ \
		$(use_with cxx) \
		$(use_enable gengc) \
		$(use_enable precisegc) \
		$(use_enable debug) \
		$(use_with debug debug-cflags) \
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

	## for /proc/self/clear_refs (see #867052)
	addpredict /proc

	emake
}

src_install() {
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

# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Nov 10 15:43:45 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 201110 Update to llvm 10 1.8.17->1.8.20
# ;use /usr/local/texlive/ via env.d
# ;madhu 221214 1.9.5
# ;madhu 230729 1.9.7-r1
# ;madhu 241209 1.12.0, -fpermissive for libfmt-11.0.2 , bundle MAX_ITEMS_BEFORE_X_INDEX patch from pulll#9194

EAPI=8

LLVM_COMPAT=( 14 )
LLVM_OPTIONAL=1
PYTHON_COMPAT=( python3_{10..13} )
PYTHON_REQ_USE="xml(+)"

inherit cmake flag-o-matic llvm python-any-r1

DESCRIPTION="Documentation system for most programming languages"
HOMEPAGE="https://www.doxygen.nl/"

if [[ ${PV} == *9999* ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/doxygen/doxygen.git"
else
	SRC_URI="https://doxygen.nl/files/${P}.src.tar.gz"
	SRC_URI+=" https://downloads.sourceforge.net/doxygen/rel-${PV}/${P}.src.tar.gz"
	KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~x64-solaris"
fi

# GPL-2 also for bundled libmscgen
LICENSE="GPL-2"
SLOT="0"
IUSE="clang debug doc dot doxysearch gui test"
# - We need TeX for tests, bug #765472
# - We keep the odd construct of noop USE=test because of
#   the special relationship b/t RESTRICT & USE for tests.
#   Also, it's a hint which avoids tests being silently skipped during arch testing.
REQUIRED_USE="clang? ( ${LLVM_REQUIRED_USE} ) test? ( doc )"
RESTRICT="!test? ( test )"

#"	app-alternatives/yacc
#	app-alternatives/lex
#"

BDEPEND="
	sys-devel/bison
	sys-devel/flex
	${PYTHON_DEPS}
"

# 	doc? (
# 		dev-texlive/texlive-bibtexextra
# 		dev-texlive/texlive-fontsextra
# 		dev-texlive/texlive-fontutils
# 		dev-texlive/texlive-latex
# 		dev-texlive/texlive-latexextra
# 		dev-texlive/texlive-plaingeneric
# 	)
#	clang? (
#		$(llvm_gen_dep '
#			sys-devel/clang:${LLVM_SLOT}=
#			sys-devel/llvm:${LLVM_SLOT}=
#		')
#	)
# 		media-gfx/graphviz[freetype(+)]

RDEPEND="
	app-text/ghostscript-gpl
	dev-db/sqlite:3
	dev-lang/perl
	dev-libs/libfmt:=
	dev-libs/spdlog:=
	virtual/libiconv
	clang? ( >=sys-devel/clang-10:= )
	dot? (
		media-gfx/graphviz
		media-libs/freetype
	)
	doxysearch? ( dev-libs/xapian:= )
	gui? (
		dev-qt/qtcore:5
		dev-qt/qtgui:5
		dev-qt/qtwidgets:5
		dev-qt/qtxml:5
	)
"
DEPEND="${RDEPEND}"

PATCHES=(
	"${FILESDIR}/${PN}-1.9.4-link_with_pthread.patch"
#	"${FILESDIR}/${PN}-1.9.1-ignore-bad-encoding.patch"
#	"${FILESDIR}/${PN}-1.9.1-do_not_force_libcxx.patch"
#	"${FILESDIR}/${PN}-1.9.7-musl-1.2.4.patch"
#	"${FILESDIR}/${PN}-1.12.0-libfmt-11.patch"
	# Backports
#	"${FILESDIR}/${PN}-1.12.0-clang-19.patch"
	${FILESDIR}/doxygen-1.12.0-make-MAX_ITEMS_BEFORE_-MULTIPAGE-QUICK-_INDEX-config.patch

)

DOCS=( LANGUAGE.HOWTO README.md )

pkg_setup() {
	use clang && llvm_pkg_setup
	python-any-r1_pkg_setup
}

src_prepare() {
	cmake_src_prepare

	# Call dot with -Teps instead of -Tps for EPS generation - bug #282150
	sed -i -e '/addJob("ps"/ s/"ps"/"eps"/g' src/dot.cpp || die

	# fix pdf doc
	sed -i.orig -e "s:g_kowal:g kowal:" \
		doc/maintainers.txt || die

	if is-flagq "-O3" ; then
		# TODO: Investigate this and report a bug accordingly...
		ewarn "Compiling with -O3 is known to produce incorrectly"
		ewarn "optimized code which breaks doxygen. Using -O2 instead."
		replace-flags "-O3" "-O2"
	fi
	# ;madhu 241209 --- work around fmt bs
	append-cxxflags -fpermissive
}

src_configure() {
	# Very slow to compile, bug #920092
	filter-flags -fipa-pta
	# -Wodr warnings, see bug #854357 and https://github.com/doxygen/doxygen/issues/9287
	filter-lto

	local mycmakeargs=(
		-Duse_libclang=$(usex clang)
		# Let the user choose instead, see also bug #822615
		-Duse_libc++=OFF
		-Dbuild_doc=$(usex doc)
		-Dbuild_search=$(usex doxysearch)
		-Dbuild_wizard=$(usex gui)
		-Duse_sys_spdlog=ON
		-Duse_sys_sqlite3=ON
		-DBUILD_SHARED_LIBS=OFF
		-DGIT_EXECUTABLE="false"

		# Noisy and irrelevant downstream
		-Wno-dev
	)

	use doc && mycmakeargs+=(
		-DDOC_INSTALL_DIR="share/doc/${P}"
	)

	cmake_src_configure
}

src_compile() {
	cmake_src_compile

	if use doc; then
		export VARTEXFONTS="${T}/fonts" # bug #564944

		if ! use dot; then
			sed -i -e "s/HAVE_DOT               = YES/HAVE_DOT    = NO/" \
				{testing/Doxyfile,doc/Doxyfile} \
				|| die "disabling dot failed"
		fi

		# -j1 for bug #770070
		cmake_src_compile docs -j1
	fi
}

src_install() {
	cmake_src_install

	# manpages are only automatically installed when docs are
	# https://github.com/doxygen/doxygen/pull/10647
	doman doc/doxygen.1
	use gui && doman doc/doxywizard.1
	use doxysearch && {
		doman doc/doxyindexer.1
		doman doc/doxysearch.1
	}
}

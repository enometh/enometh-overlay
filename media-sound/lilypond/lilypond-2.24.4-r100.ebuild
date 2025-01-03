# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Dec 31 09:59:17 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 241231 - 2.24.4-r100 -- skip new guile eclass, upgdrade fontforge (old --version required X), move kpsewhich out of the way. tex pkgs: fontware (pltotf) ,lh,fontinst,metapost.

EAPI=8

GUILE_REQ_USE="deprecated,regex"
GUILE_COMPAT=( 2-2 3-0 )
PYTHON_COMPAT=( python3_{10..13} )

inherit elisp-common autotools guile-single python-single-r1 toolchain-funcs xdg-utils

if [[ ${PV} == *9999* ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://git.savannah.gnu.org/git/lilypond.git"
else
	MAIN_VER=$(ver_cut 1-2)
	SRC_URI="https://lilypond.org/download/sources/v${MAIN_VER}/${P}.tar.gz"
	KEYWORDS="amd64 ~arm arm64 ~hppa ~riscv x86"
fi

DESCRIPTION="GNU Music Typesetter"
HOMEPAGE="http://lilypond.org/"

LICENSE="GPL-3 FDL-1.3"
SLOT="0"
LANG_USE="l10n_ca l10n_cs l10n_de l10n_en l10n_fr l10n_hu l10n_it l10n_ja l10n_nl l10n_pt l10n_zh"
IUSE="debug doc emacs profile ${LANG_USE}"
unset LANG_USE
#REQUIRED_USE="${GUILE_REQUIRED_USE} ${PYTHON_REQUIRED_USE}"
REQUIRED_USE="${PYTHON_REQUIRED_USE}"

BDEPEND="
	dev-texlive/texlive-metapost
	sys-apps/texinfo
	app-alternatives/yacc
	app-alternatives/lex
	virtual/pkgconfig
	doc? ( app-text/texi2html )
"

# 	${GUILE_DEPS}

RDEPEND="app-text/ghostscript-gpl
	dev-libs/boehm-gc
	dev-libs/glib:2
	dev-libs/libatomic_ops
	media-fonts/tex-gyre
	media-libs/fontconfig
	media-libs/freetype:2
	media-libs/harfbuzz
	>=x11-libs/pango-1.40
	emacs? ( >=app-editors/emacs-23.1:* )
	>=dev-scheme/guile-2.2:12=[regex]
	${PYTHON_DEPS}"
DEPEND="${RDEPEND}
	app-text/t1utils
	dev-lang/perl
	dev-libs/kpathsea
	media-gfx/fontforge[png,python]
	sys-devel/gettext
	doc? (
		dev-texlive/texlive-langcyrillic
		l10n_cs? ( dev-texlive/texlive-xetex )
		l10n_ja? ( dev-texlive/texlive-langjapanese )
		l10n_zh? ( dev-texlive/texlive-langchinese )
	)
"
# Correct output data for tests isn't bundled with releases
RESTRICT="test"

DOCS=( DEDICATION README.md ROADMAP )

QA_PREBUILT="usr/*/${PN}/${PV}/ccache/*"

pkg_setup() {
	guile-single_pkg_setup
	python-single-r1_pkg_setup
}

src_prepare() {
	guile-single_src_prepare

	# respect CFLAGS
	sed -i 's/OPTIMIZE -g/OPTIMIZE/' aclocal.m4 || die

	eautoreconf

	xdg_environment_reset #586592
}

src_configure() {
	# fix hardcoded `ar`
	sed -i "s/AR=ar/AR=$(tc-getAR)/g" flower/GNUmakefile || die "Failed to fix ar command"

	local myeconfargs=(
		--disable-optimising
		--disable-pipe
		$(use_enable debug debugging)
		$(use_enable doc documentation)
		$(use_enable profile profiling)
	)

	export VARTEXFONTS="${T}/fonts"  # https://bugs.gentoo.org/692010

	econf "${myeconfargs[@]}" AR="$(tc-getAR)"
}

src_compile() {
	default

	# http://lilypond.org/doc/v2.24/Documentation/changes/index#notes-for-source-compilation-and-packagers
	emake bytecode

	use doc && emake LANGS="${L10N}" doc info

	if use emacs ; then
		elisp-compile elisp/lilypond-{font-lock,indent,mode,what-beat}.el \
			|| die "elisp-compile failed"
	fi
}

src_install() {
	emake DESTDIR="${D}" vimdir=/usr/share/vim/vimfiles install install-bytecode

	use doc && emake DESTDIR="${D}" install-doc

	# remove elisp files since they are in the wrong directory
	rm -r "${ED}"/usr/share/emacs || die

	if use emacs ; then
		elisp-install ${PN} elisp/*.{el,elc} elisp/out/*.el \
			|| die "elisp-install failed"
		elisp-site-file-install "${FILESDIR}"/50${PN}-gentoo.el
	fi

	guile_unstrip_ccache

	python_fix_shebang "${ED}"

	einstalldocs
}

pkg_postinst() {
	use emacs && elisp-site-regen
}

pkg_postrm() {
	use emacs && elisp-site-regen
}

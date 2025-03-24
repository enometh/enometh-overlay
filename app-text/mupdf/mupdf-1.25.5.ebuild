# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Jan 30 13:31:27 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190130 - experiment with a git source
# ;madhu 190715 - 1.15.0 - curl gone. mujs and freeglut are bundled
# ;madhu 191225 - 1.16.1 rebased
#
# ;madhu 191225 - 1.16.1-r1 - use the pdf and bundled patches from
# rodistdir: patches unpack into ${WORKDIR}/enometh-patches/
#(cd ~/scratch/extern/mupdf-1.16.1-source ;git checkout madhu-1.16.1; rm -fv 00*; git format-patch vendor; rm -rfv enometh-patches; mkdir enometh-patches -pv; mv 00* enometh-patches/ -fv;  tar cvfJ ~/scratch/mirrors/myrodistdir/mupdf-1.16.1-enometh-patches.tar.xz enometh-patches/) &
# ;madhu 210218 1.18.0-r2 git again
# ;madhu 211011 1.19.0
# ;madhu 220816 1.20.3
# ;madhu 230306 1.21.1-r1
# ;madhu 250325 1.25.5 - shared, use bundled tesseract,leptonica, no GENTOO_PV

EAPI=8

USE_GIT=true

# Please check upstream git regularly for relevant security-related commits
# to backport.

inherit desktop flag-o-matic toolchain-funcs xdg

DESCRIPTION="A lightweight PDF viewer and toolkit written in portable C"
HOMEPAGE="https://mupdf.com/ https://git.ghostscript.com/?p=mupdf.git"

if ${USE_GIT} ; then
	inherit git-r3
	EGIT_REPO_URI="https://example.com/mupdf.git"
	EGIT_MIRROR_URI="file:///build/git-mirror"
	EGIT_CLONE_TYPE="shallow"
	EGIT_BRANCH="tmp-madhu-1.25.5"
	EGIT_SUBMODULES=()
	SRC_URI=""
	S=${EGIT_CHECKOUT_DIR}
else
# ;madhu 230306 xz -> lz upstream
SRC_URI="https://mupdf.com/downloads/archive/${P}-source.tar.gz"
S="${WORKDIR}/${P}-source"
#SRC_URI+=" https://example.com/${PN}-${PV}-enometh-patches.tar.xz"
fi

LICENSE="AGPL-3"
SLOT="0/${PV}"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~loong ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86"
IUSE="+drm archive +javascript opengl ssl X"
REQUIRED_USE="opengl? ( javascript )"

# Although we use the bundled, patched version of freeglut in mupdf (because of
# bug #653298), the best way to ensure that its dependencies are present is to
# install system's freeglut.
RDEPEND="
	archive? ( app-arch/libarchive )
	dev-libs/gumbo:=
	media-libs/freetype:2
	media-libs/harfbuzz:=[truetype]
	media-libs/jbig2dec:=
	media-libs/libpng:0=
	>=media-libs/openjpeg-2.1:2=
	>=media-libs/libjpeg-turbo-1.5.3-r2:0=
	net-misc/curl
	javascript? ( >=dev-lang/mujs-1.2.0:= )
	opengl? ( >=media-libs/freeglut-3.0.0 )
	ssl? ( >=dev-libs/openssl-1.1:0= )
	sys-libs/zlib
	X? (
		x11-libs/libX11
		x11-libs/libXext
		x11-libs/libXrandr
	)
"
# 		media-libs/libglvnd[X]

DEPEND="${RDEPEND}
	X? ( x11-base/xorg-proto )"
BDEPEND="virtual/pkgconfig"

if ! ${USE_GIT}; then
die "TODO NO GENTOO PATCHES"
PATCHES=(
0001-thirdparty-mujs-jsi.h-moon-fix-compile-for-NAN-isfin.patch
0002-platform-x11-x11_main.c-winopen-support-XEMBED-envir.patch
0003-platform-x11-pdfapp.c-pan-contiguously-for-pgup-pgdn.patch
0004-platform-x11-pdfapp.c-pan-to-multiple-search-strings.patch
0005-platform-x11-pdfapp.-ch-enhanced-snapback-history-ne.patch
0006-platform-x11-x11_main.c-translate-XK_space-to-pgdn.patch
0007-platform-x11-pdfapp.c-accept-numerical-args-for-hjkl.patch
0008-platform-x11-x11_main.c-swap_button_hack.patch
0009-platform-x11-pdfapp.c-do-not-change-pages-when-scrol.patch
0010-cmdfifo-patch-handle-c-argument.-c-tmp-server-name-s.patch
0011-platform-gl-gl-main.c-avoid-segfault-on-show-js-cons.patch
0012-Makefile-adjust-default-build-to-debug.patch
0013-include-mupdf-fitz-config.h-Enable-defines-to-AVOID-.patch
0014-platform-x11-pdfapp.c-pdfapp_onkey-Save-numbers-type.patch
0015-platform-x11-x11_main.c-Add-Control-Modifiers-for-KP.patch
0016-platform-x11-pdfapp.c-pdfapp_onkey-rename-fullscreen.patch
0017-platform-x11-pdfapp.c-fix-hjkl-contiguous-scroll.patch
0018-platform-x11-pdfapp.c-pdfapp_onkey-implement-smart-f.patch
0019-src-pdf-pdf-xref.c-reinstate-pdf_recognize.patch
0020-mupdf-1.21.0-add-desktop-pc-files.patch.patch
0021-mupdf-1.15-CFLAGS.patch.patch
0022-mupdf-1.19-Makefile.patch.patch
0023-platform-x11-x11_main.c-onselreq-fix-typo.patch
0024-platform-debian-mupdf.desktop-prefer-Icon-mupdf.patch
0025-Makefile-revert-gentoo-changes-build-a-shared-librar.patch
0026-platform-x11-x11_main.c-tmp-debugging.patch
0027-k2pdfopt_v2.54-mupdf_mod.patch
0028-k2pdfopt-patches-fix-lcms.h.patch
0029-Makerules-fPIC-when-building-static-libraries.patch
0030-tmp-debugging-how-tesseract-leptonica-get-included.patch
0031-tmp-debugging-source-pdf-font-win32.c-pdf_install_lo.patch
0032-background-transparancy-patch.patch
0033-Makerules-fix-bundled-GLUT-to-use-xorg-xinput-cflags.patch
)
fi

src_prepare() {
	default

	use hppa && append-cflags -ffunction-sections

	append-cflags "-DFZ_ENABLE_JS=$(usex javascript 1 0)"

	sed -e "1iOS = Linux" \
		-e "1iCC = $(tc-getCC)" \
		-e "1iCXX = $(tc-getCXX)" \
		-e "1iLD = $(tc-getLD)" \
		-e "1iAR = $(tc-getAR)" \
		-e "1iverbose = yes" \
		-e "1ibuild = debug" \
		-i Makerules || die "Failed adding build variables to Makerules in src_prepare()"

	# Adjust MuPDF version in .pc file created by the
	# [...]-add-desktop-pc-files.patch file
	sed -e "s/Version: \(.*\)/Version: ${PV}/" \
		-i platform/debian/${PN}.pc || die "Failed substituting version in ${PN}.pc"
}

_emake() {
	# When HAVE_OBJCOPY is yes, we end up with a lot of QA warnings.
	#
	# Bundled libs
	# * General
	# Note that USE_SYSTEM_LIBS=yes is a metaoption which will set to upstream's
	# recommendations. It does not mean "always use system libs".
	# See [0] below for what it means in a specific version.
	#
	# * freeglut
	# We don't use system's freeglut because upstream has a special modified
	# version of it that gives mupdf clipboard support. See bug #653298
	#
	# * mujs
	# As of v1.15.0, mupdf started using symbols in mujs that were not part
	# of any release. We then went back to using the bundled version of it.
	# But v1.17.0 looks ok, so we'll go unbundled again. Be aware of this risk
	# when bumping and check!
	# See bug #685244
	#
	# * lmms2
	# mupdf uses a bundled version of lcms2 [0] because Artifex have forked it [1].
	# It is therefore not appropriate for us to unbundle it at this time.
	#
	# [0] https://git.ghostscript.com/?p=mupdf.git;a=blob;f=Makethird;h=c4c540fa4a075df0db85e6fdaab809099881f35a;hb=HEAD#l9
	# [1] https://www.ghostscript.com/doc/lcms2mt/doc/WhyThisFork.txt
	local myemakeargs=(
#		GENTOO_PV=${PV}
		shared=yes
		USE_SYSTEM_LEPTONICA=no
		USE_SYSTEM_TESSERACT=no
		tesseract=yes
		leptonica=yes
		verbose=yes
		HAVE_GLUT=$(usex opengl)
		HAVE_LIBCRYPTO=$(usex ssl)
		HAVE_X11=$(usex X)
		USE_SYSTEM_LIBS=yes
		USE_SYSTEM_MUJS=$(usex javascript)
		USE_SYSTEM_GLUT=no
		HAVE_OBJCOPY=no
		"$@"
	)

	emake "${myemakeargs[@]}"
}

src_compile() {
	tc-export PKG_CONFIG

	_emake XCFLAGS="-fPIC"
}

src_install() {
	if use opengl || use X ; then
		domenu platform/debian/${PN}.desktop
		doicon -s scalable docs/logo/new-${PN}-icon.svg
	else
		rm docs/man/${PN}.1 || die "Failed to remove man page in src_install()"
	fi

	sed -i \
		-e "1iprefix = ${ED}/usr" \
		-e "1ilibdir = ${ED}/usr/$(get_libdir)" \
		-e "1idocdir = ${ED}/usr/share/doc/${PF}" \
		-i Makerules || die "Failed adding liprefix, lilibdir and lidocdir to Makerules in src_install()"

	_emake install

	dosym libmupdf.so.${PV#1.} /usr/$(get_libdir)/lib${PN}.so

	if use opengl ; then
		einfo "mupdf symlink points to mupdf-gl (bug 616654)"
		dosym ${PN}-gl /usr/bin/${PN}
	elif use X ; then
		einfo "mupdf symlink points to mupdf-x11 (bug 616654)"
		dosym ${PN}-x11 /usr/bin/${PN}
	fi

	# Respect libdir and EPREFIX (bugs #734898, #911965)
	sed -i -e "s:/lib:/$(get_libdir):" \
		-e "s:/usr:${EPREFIX}/usr:" platform/debian/${PN}.pc \
		|| die "Failed to sed pkgconfig file to respect libdir and EPREFIX in src_install()"

	insinto /usr/$(get_libdir)/pkgconfig
	doins platform/debian/${PN}.pc

	dodoc README CHANGES CONTRIBUTORS
}

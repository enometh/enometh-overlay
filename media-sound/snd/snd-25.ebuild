# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Mar 14 15:19:39 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250314 - snd-25
# ln -sv /16/tmp.d/mirrors.d/ccrma.stanford.edu/software/snd/snd-25.tar.bz2 /gentoo/distfiles

EAPI=8

inherit autotools flag-o-matic

DESCRIPTION="Snd is a sound editor"
HOMEPAGE="https://ccrma.stanford.edu/software/snd/"
SRC_URI="https://ccrma.stanford.edu/software/${PN}/${P}.tar.bz2"

LICENSE="Snd 0BSD BSD-2 HPND GPL-2+ LGPL-2.1+ LGPL-3+"
SLOT="0"
KEYWORDS="~amd64 ~ppc ~x86 ~amd64-linux ~x86-linux"
IUSE="alsa doc fftw gmp gsl gui jack ladspa notcurses opengl oss portaudio pulseaudio readline ruby +s7"

RDEPEND="
	alsa? ( media-libs/alsa-lib )
	fftw? ( sci-libs/fftw:3.0= )
	gmp? (
		dev-libs/gmp:=
		dev-libs/mpc
		dev-libs/mpfr:=
	)
	gsl? ( sci-libs/gsl:= )
	gui? ( x11-libs/motif:0 )
	jack? ( virtual/jack )
	ladspa? ( media-libs/ladspa-sdk )
	notcurses? ( dev-cpp/notcurses )
	opengl? ( virtual/opengl )
	portaudio? ( media-libs/portaudio )
	pulseaudio? ( media-sound/pulseaudio )
	readline? ( sys-libs/readline:= )
	ruby? ( dev-lang/ruby:* )
"
DEPEND="${RDEPEND}"
BDEPEND="virtual/pkgconfig"

REQUIRED_USE="
	?? ( portaudio pulseaudio )
	?? ( ruby s7 )"

src_prepare() {
	default

	sed -i -e "s:-O2 ::" configure.ac || die
	eautoreconf
}

src_configure() {
	# Workaround executable sections QA warning (bug #348754)
	append-ldflags -Wl,-z,noexecstack

	local myeconfargs=(
		$(use_with alsa)
		$(use_with fftw)
		$(use_with gmp)
		$(use_with gsl)
		$(use_with gui)
		$(use_with jack)
		$(use_with ladspa)
		$(use_with notcurses)
		$(use_with oss)
		$(use_with portaudio)
		$(use_with pulseaudio)
		$(use_with ruby)
		$(use_with s7)
	)

	if ! use ruby && ! use s7 ; then
		myeconfargs+=( --without-extension-language )
	fi

	econf "${myeconfargs[@]}"
}

src_compile() {
	emake snd

	# Do not compile ruby extensions for command line programs since they fail
	sed -i -e "s:HAVE_RUBY 1:HAVE_RUBY 0:" mus-config.h || die

	emake sndplay sndinfo
}

src_install() {
	dobin snd sndplay sndinfo

	if use ruby ; then
		insinto /usr/share/snd
		doins *.rb
	fi

	if use s7 ; then
		insinto /usr/share/snd
		doins *.scm
	fi

	use doc && HTML_DOCS=( *.html pix/*.png )
	einstalldocs
	dodoc HISTORY.Snd
}

# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Dec 28 20:10:51 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 201228 - guvcview-2.0.6-r1.ebuild patches from git.
# ;madhu 210901 - regular git
# ;madhu 211203 2.0.7 v2.0.7-2-5-g9edc615
EAPI=7

USE_GIT=true

MY_P=${PN}-src-${PV}
inherit autotools qmake-utils toolchain-funcs

DESCRIPTION="Simple Qt5 or GTK+3 interface for capturing and viewing video from v4l2 devices"
HOMEPAGE="http://guvcview.sourceforge.net/"
if ${USE_GIT};  then
   inherit git-r3
   EGIT_REPO_URI="https://git.code.sf.net/p/guvcview/git-master"
   #EGIT_OVERRIDE_REPO_GUVCVIEW_GIT_MASTER=file:///build/git-mirror/guvcview.git
	EGIT_CLONE_TYPE=shallow
else
SRC_URI="mirror://sourceforge/${PN}/${MY_P}.tar.gz"
fi

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="amd64 x86"
IUSE="gsl pulseaudio qt5 nls gtk3"

BDEPEND="
	dev-util/intltool
	sys-devel/autoconf-archive
	sys-devel/gettext
	virtual/pkgconfig
"
RDEPEND="
	>=dev-libs/glib-2.10
	media-libs/libpng:0=
	media-libs/libsdl2
	media-libs/libv4l
	>=media-libs/portaudio-19_pre
	>=media-video/ffmpeg-2.8:0=
	virtual/libusb:1
	virtual/udev
	gsl? ( >=sci-libs/gsl-1.15 )
	pulseaudio? ( >=media-sound/pulseaudio-0.9.15 )
	qt5? (
		dev-qt/qtcore:5
		dev-qt/qtgui:5
		dev-qt/qtwidgets:5
	)
	!qt5? ( >=x11-libs/gtk+-3.6:3 )
"
# linux-headers: bug 448260
DEPEND="${RDEPEND}
	>=sys-kernel/linux-headers-3.4-r2
	virtual/os-headers
"

if ! ${USE_GIT}; then
S="${WORKDIR}/${MY_P}"
fi

PATCHES=( ${FILESDIR}/guvcview-4.0.6-ffmpeg-4.5-support-hack-avcodec_get_context_defaults.patch )

src_prepare() {
	default
	sed -i '/^docdir/,/^$/d' Makefile.am || die
	echo "guvcview/gui_qt5_audioctrls.cpp" >> po/POTFILES.skip || die # bug 630984
	eautoreconf
}

src_configure() {
	export MOC="$(qt5_get_bindir)/moc"

	# 599030
	tc-export CC CXX

	local myeconfargs=(
		--disable-debian-menu
		--disable-static
		$(use_enable nls)
		$(use_enable gsl)
		$(use_enable pulseaudio pulse)
		$(use_enable qt5)
		$(use_enable gtk3)
	)
	econf "${myeconfargs[@]}"
}

src_install() {
	default
	find "${D}" -name '*.la' -type f -delete || die
}

# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed May 22 17:26:58 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190522 - 1.5.0_rc2-r2 1.5.0-RC-1016-g13a32119
# ;madhu 240301 - 1.5.0-15-gb71db345 (requires discontinued qtwebkit, rebuilds indexes. libav not tested)

EAPI=8
USE_GIT=true

if ! ${USE_GIT}; then
MY_PV=${PV^^}
MY_PV=${MY_PV/_/-}
fi

inherit desktop qmake-utils

DESCRIPTION="Feature-rich dictionary lookup program"
HOMEPAGE="http://goldendict.org/"
if ${USE_GIT}; then
	inherit git-r3
	SRC_URI=""
	EGIT_REPO_URI="https://github.com/goldendict/goldendict"
	#EGIT_OVERRIDE_REPO_GOLDENDICT_GOLDENDICT=file:///build/git-mirrors/goldendict
	#EGIT_OVERRIDE_REPO_TVANGESTE_GOLDENDICT_WINLIBS_PREBUILT=file:///build/git-mirrors/extern/goldendict-winlibs-prebuilt
else
	SRC_URI="https://github.com/${PN}/${PN}/archive/${MY_PV}.tar.gz -> ${P}.tar.gz"
fi

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="amd64 x86"
IUSE="debug ffmpeg libav"

RDEPEND="
	app-arch/bzip2
	>=app-text/hunspell-1.2:=
	dev-libs/eb
	dev-libs/lzo
	dev-qt/qtcore:5
	dev-qt/qtgui:5
	dev-qt/qthelp:5
	dev-qt/qtnetwork:5
	dev-qt/qtprintsupport:5
	dev-qt/qtsingleapplication[qt5(+),X]
	dev-qt/qtsvg:5
	dev-qt/qtwebkit:5
	dev-qt/qtwidgets:5
	dev-qt/qtx11extras:5
	dev-qt/qtxml:5
	media-libs/libvorbis
	media-libs/tiff:0
	sys-libs/zlib
	x11-libs/libX11
	x11-libs/libXtst
	ffmpeg? (
		media-libs/libao
		libav? ( media-video/libav:0= )
		!libav? ( media-video/ffmpeg:0= )
	)
"
DEPEND="${RDEPEND}"
BDEPEND="
	dev-qt/linguist-tools:5
	virtual/pkgconfig
"

PATCHES=(
	"${FILESDIR}/${PN}-1.5.0-qtsingleapplication-unbundle.patch"
# ;madhu 190522 - these fail
#	"${FILESDIR}/${PN}-1.5.0-qt-5.11.patch"
#	"${FILESDIR}/${PN}-1.5.0-ffmpeg-4.patch"
)

if ! ${USE_GIT}; then
S="${WORKDIR}/${PN}-${MY_PV}"
fi

src_prepare() {
	default

if ! ${USE_GIT}; then
	# disable git
	sed -i \
		-e '/git describe/s/^/#/' \
		${PN}.pro || die
fi
	# fix installation path
	sed -i \
		-e '/PREFIX = /s:/usr/local:/usr:' \
		${PN}.pro || die

	# kill translations
	sed -i -e '/locale.*\.ts/d' goldendict.pro
	rm -fv locale/*.ts

	# add trailing semicolon (;madhu 240301 fixed upsteam)
	# sed -i -e '/^Categories/s/$/;/' redist/org.goldendict.GoldenDict.desktop || die
}

src_configure() {
	local myconf=(goldendict.pro)
	use ffmpeg || myconf+=( DISABLE_INTERNAL_PLAYER=1 )

	eqmake5 "${myconf[@]}"
}

src_install() {
	dobin ${PN}
	domenu redist/org.goldendict.GoldenDict.desktop
	doicon redist/icons/${PN}.png

	insinto /usr/share/metainfo
	doins redist/org.goldendict.GoldenDict.metainfo.xml

# madhu: no locale
#	insinto /usr/share/apps/${PN}/locale
#	doins locale/*.qm
#
#	insinto /usr/share/${PN}/help
	doins help/gdhelp_en.qch
}

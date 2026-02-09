# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue May 16 18:45:18 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 210410 - pulseeffects-5.0.3
# ;madhu 230516 - 7.0.4, src_configure: work around 11.2 gcc error
# ;madhu 260209 - 8.1.2, now with kde, alt Qt-6.8.1

EAPI=8

KFMIN=6.10
QTVER=6.8.1
inherit ecm optfeature toolchain-funcs xdg

DESCRIPTION="Limiter, auto volume and many other plugins for PipeWire applications"
HOMEPAGE="https://github.com/wwmm/easyeffects"

if [[ ${PV} == *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/wwmm/easyeffects"
else
	SRC_URI="https://github.com/wwmm/easyeffects/archive/v${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="~amd64"
fi

LICENSE="GPL-3"
SLOT="0"

IUSE="webengine"
# No real tests. ECM brings appstream test which isn't relevant downstream.
RESTRICT="test"

#	>=media-video/pipewire-1.0.6:=[sound-server]
RDEPEND="
	dev-cpp/nlohmann_json
	dev-cpp/tbb:=
	dev-libs/glib:2
	dev-libs/kirigami-addons:6
	dev-libs/libportal:=[qt6]
	dev-qt/qtbase:6[dbus,gui,network,widgets]
	dev-qt/qtdeclarative:6
	dev-qt/qtgraphs:6
	>=kde-frameworks/kcolorscheme-${KFMIN}:6
	>=kde-frameworks/kconfig-${KFMIN}:6
	>=kde-frameworks/kconfigwidgets-${KFMIN}:6
	>=kde-frameworks/kcoreaddons-${KFMIN}:6
	>=kde-frameworks/ki18n-${KFMIN}:6
	>=kde-frameworks/kiconthemes-${KFMIN}:6
	>=kde-frameworks/kirigami-${KFMIN}:6
	>=kde-frameworks/qqc2-desktop-style-${KFMIN}:6
	media-libs/libbs2b
	>=media-libs/libebur128-1.2.6:=
	media-libs/libsamplerate
	media-libs/libsndfile
	media-libs/libsoundtouch:=
	>=media-libs/lilv-0.24
	media-libs/rnnoise
	media-libs/speexdsp
	media-libs/webrtc-audio-processing:2
	>=media-libs/zita-convolver-3.0.0:=
	>=media-video/pipewire-1.0.6
	sci-libs/fftw:3.0=
	sci-libs/gsl:=
	webengine? ( dev-qt/qtwebengine:6[qml] )
"
DEPEND="${RDEPEND}
	media-libs/ladspa-sdk
"
BDEPEND="
	dev-libs/appstream
	sys-devel/gettext
	virtual/pkgconfig
"

PATCHES=(
	${FILESDIR}/easyeffects-8.0.5-qstring-arg-stdstring.patch
)

pkg_pretend() {
	if [[ ${MERGE_TYPE} != "binary" ]] ; then
		if ! tc-is-gcc; then
			if ! tc-is-clang || [[ $(clang-major-version) -lt 16 ]]; then
				die "${PN} can only be built with GCC or >=Clang-16 due to required level of C++20 support"
			fi
		elif [[ $(gcc-major-version) -lt 11 ]] ; then
			die "Since version 6.2.5, ${PN} requires GCC 11 or newer to build (bug #848072)"
		fi
	fi
}

src_prepare() {
	default
	cmake_prepare
	find po -type f -exec rm -fv '{}' ';'
	echo > po_news/LINGUAS
}

src_configure() {
	local libcxx=false
	[[ $(tc-get-cxx-stdlib) == "libc++" ]] && libcxx=true

	local mycmakeargs=(
		-DENABLE_LIBCPP_WORKAROUNDS=${libcxx}
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	ecm_src_configure
}

pkg_postinst() {
	xdg_pkg_postinst

	optfeature_header "Install optional audio plugins:"
	optfeature "limiter, exciter, bass enhancer and others" media-plugins/calf
	optfeature "equalizer, compressor, delay, loudness" media-libs/lsp-plugins
	optfeature "maximizer" media-plugins/zam-plugins
	optfeature "bass loudness" media-plugins/mda-lv2
	optfeature "noise remover (available in GURU overlay)" media-sound/deep-filter[ladspa]
}

# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Dec 30 10:22:22 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 201230 obs-studio-25.0.8-r100 -> 26.1.0, patch for python3.8
# remove locale files, patch to hardcode alsa to avoid a segfault
# ;madhu 210616 27.0.1
# ;madhu 230630 27.0.1 - fail
# ;madhu 230630 29.1.3 ENABLE_SERVICE_UPDATES=OFF
# ;madhu 231010 29.1.3-r1 -- fudge use flags for qt5gui[wayland]
# -- requires rebuild of qtgui, mesa, libva,
# ;madhu 241205 31.0.0_rc1 NON STANDARD QT6_ROOT FFMPEG_ROOT, SWIG4_ROOT, specialcase pipewire, handle EGIT_SUBMODULES which don't work when the git repo doesnt have submodule commits.
# ln -sv /16/tmp.d/mirrors.d/github.com/obsproject/obs-studio/archive/refs/tags/31.0.0-rc1.tar.gz /gentoo/distfiles/obs-studio-31.0.0_rc1.tar.gz

EAPI=8

CMAKE_REMOVE_MODULES_LIST=( FindFreetype )
LUA_COMPAT=( luajit )
PYTHON_COMPAT=( python3_{9..12} )

USE_GIT=true
# XXX XXX XXX
QT6_ROOT=/opt/Qt6.8.1
FFMPEG_ROOT=/opt/ffmpeg
SWIG4_ROOT=/opt/swig4
SWIG4_PV=4.2.1

inherit cmake flag-o-matic lua-single optfeature python-single-r1 xdg

CEF_DIR="cef_binary_5060_linux_x86_64"
CEF_REVISION="_v3"
OBS_BROWSER_COMMIT="082a0a2d1c393f66dc68b62fa402ca23d4c02dbe"
OBS_WEBSOCKET_COMMIT="eed8a49933786383d11f4868a4e5604a9ee303c6"

DESCRIPTION="Software for Recording and Streaming Live Video Content"
HOMEPAGE="https://obsproject.com"

if [[ ${PV} == 9999  ]] || ${USE_GIT} ; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/obsproject/obs-studio.git"
	EGIT_SUBMODULES=(
		plugins/obs-browser
		plugins/obs-websocket
	)
#bogus
#	SRC_URI="https://github.com/obsproject/${PN}/archive/${PV}.tar.gz -> ${P}.tar.gz"
	EGIT_CLONE_TYPE=shallow
	EGIT_BRANCH=master

# this should go to portateg/env/obs-studio-egit-overrides
	EGIT_OVERRIDE_REPO_OBSPROJECT_OBS_STUDIO=file:///build/git-mirror/obs-studio.git
	EGIT_OVERRIDE_REPO_OBSPROJECT_OBS_BROWSER=file:///build/git-mirror/obs-browser.git
	EGIT_OVERRIDE_REPO_OBSPROJECT_OBS_WEBSOCKET=file:///build/git-mirror/obs-websocket.git
	EGIT_OVERRIDE_BRANCH_OBSPROJECT_OBS_BROWSER=master
	EGIT_OVERRIDE_BRANCH_OBSPROJECT_OBS_WEBSOCKET=master

	KEYWORDS="~amd64 ~arm64 ~ppc64 ~x86"

else
	SRC_URI="
		https://github.com/obsproject/${PN}/archive/${PV}.tar.gz -> ${P}.tar.gz
		https://github.com/obsproject/obs-browser/archive/${OBS_BROWSER_COMMIT}.tar.gz -> obs-browser-${OBS_BROWSER_COMMIT}.tar.gz
		https://github.com/obsproject/obs-websocket/archive/${OBS_WEBSOCKET_COMMIT}.tar.gz -> obs-websocket-${OBS_WEBSOCKET_COMMIT}.tar.gz
	"
	KEYWORDS="~amd64 ~arm64 ~ppc64 ~x86"
fi

SRC_URI+=" browser? ( https://cdn-fastly.obsproject.com/downloads/${CEF_DIR}${CEF_REVISION}.tar.xz )"

LICENSE="Boost-1.0 GPL-2+ MIT Unlicense"
SLOT="0"
IUSE="
	+alsa browser decklink fdk jack lua mpegts nvenc pipewire pulseaudio
	python qsv speex +ssl test truetype v4l vlc wayland websocket
"
RESTRICT="!test? ( test )"
REQUIRED_USE="
	browser? ( || ( alsa pulseaudio ) )
	lua? ( ${LUA_REQUIRED_USE} )
	python? ( ${PYTHON_REQUIRED_USE} )
"

BDEPEND="
	lua? ( dev-lang/swig )
	python? ( dev-lang/swig )
"
# media-video/ffmpeg[opus] required due to bug 909566
#	media-libs/libglvnd[X]
#	dev-qt/qtbase:6[network,widgets,xml(+)]
#	dev-qt/qtsvg:6
# 	pulseaudio? ( media-libs/libpulse )
DEPEND="
	dev-cpp/nlohmann_json
	dev-libs/glib:2
	dev-libs/jansson:=
	dev-libs/uthash
	media-libs/libva
	media-libs/rnnoise
	media-libs/x264:=
	media-video/ffmpeg:=[nvenc?,opus,x264]
	net-misc/curl
	sys-apps/dbus
	sys-apps/pciutils
	sys-apps/util-linux
	sys-libs/zlib:=
	x11-libs/libX11
	x11-libs/libxcb:=
	x11-libs/libXcomposite
	x11-libs/libXfixes
	x11-libs/libxkbcommon
	alsa? ( media-libs/alsa-lib )
	browser? (
		|| (
			>=app-accessibility/at-spi2-core-2.46.0:2
			( app-accessibility/at-spi2-atk dev-libs/atk )
		)
		dev-libs/expat
		dev-libs/glib
		dev-libs/nspr
		dev-libs/nss
		dev-libs/wayland
		media-libs/alsa-lib
		media-libs/fontconfig
		media-libs/mesa[gbm(+)]
		net-print/cups
		x11-libs/cairo
		x11-libs/libdrm
		x11-libs/libXcursor
		x11-libs/libXdamage
		x11-libs/libXext
		x11-libs/libXi
		x11-libs/libxkbcommon
		x11-libs/libXrandr
		x11-libs/libXrender
		x11-libs/libXScrnSaver
		x11-libs/libxshmfence
		x11-libs/libXtst
		x11-libs/pango
	)
	fdk? ( media-libs/fdk-aac:= )
	jack? ( virtual/jack )
	lua? ( ${LUA_DEPS} )
	mpegts? (
		net-libs/librist
		net-libs/srt
	)
	nvenc? ( >=media-libs/nv-codec-headers-12 )
	pipewire? ( media-video/pipewire:= )
	pulseaudio? ( media-sound/pulseaudio )
	python? ( ${PYTHON_DEPS} )
	qsv? ( media-libs/libvpl )
	speex? ( media-libs/speexdsp )
	ssl? ( net-libs/mbedtls:= )
	test? ( dev-util/cmocka )
	truetype? (
		media-libs/fontconfig
		media-libs/freetype
	)
	v4l? (
		media-libs/libv4l
		virtual/udev
	)
	vlc? ( media-video/vlc:= )
	wayland? (
		dev-libs/wayland
		x11-libs/libxkbcommon
	)
	websocket? (
		dev-cpp/asio
		dev-cpp/websocketpp
		dev-libs/qr-code-generator
	)
"
RDEPEND="${DEPEND}
	qsv? ( media-libs/intel-mediasdk )
"

QA_PREBUILT="
	usr/lib*/obs-plugins/chrome-sandbox
	usr/lib*/obs-plugins/libcef.so
	usr/lib*/obs-plugins/libEGL.so
	usr/lib*/obs-plugins/libGLESv2.so
	usr/lib*/obs-plugins/libvk_swiftshader.so
	usr/lib*/obs-plugins/libvulkan.so.1
	usr/lib*/obs-plugins/swiftshader/libEGL.so
	usr/lib*/obs-plugins/swiftshader/libGLESv2.so
"

pkg_setup() {
	use lua && lua-single_pkg_setup
	use python && python-single-r1_pkg_setup
}

src_unpack() {
	default

	if [[ ${PV} == 9999 ]] || ${USE_GIT}; then
		git-r3_src_unpack
		if ${USE_GIT}; then
			# ;madhu 241206 the submodules aren't checked out because we
			# built the repo from a tarball without submodule commits.

			EGIT_BRANCH=master EGIT_COMMIT=$OBS_BROWSER_COMMIT EGIT_REPO_URI="https://github.com/obsproject/obs-browser.git" EGIT_CHECKOUT_DIR=${S}/plugins/obs-browser git-r3_src_unpack
			EGIT_BRANCH=master EGIT_COMMIT=$OBS_WEBSOCKET_COMMIT EGIT_REPO_URI="https://github.com/obsproject/obs-websocket.git" EGIT_CHECKOUT_DIR=${S}/plugins/obs-websocket git-r3_src_unpack
		fi

	else
		rm -d ${P}/plugins/obs-browser || die
		mv obs-browser-${OBS_BROWSER_COMMIT} ${P}/plugins/obs-browser || die

		rm -d ${P}/plugins/obs-websocket || die
		mv obs-websocket-${OBS_WEBSOCKET_COMMIT} ${P}/plugins/obs-websocket || die
	fi
}

src_prepare() {
	default

#	sed -i '/-Werror$/d' "${WORKDIR}"/${P}/cmake/Modules/CompilerConfig.cmake || die

	# -Werror=lto-type-mismatch
	# https://bugs.gentoo.org/867250
	# https://github.com/obsproject/obs-studio/issues/8988
	use wayland && filter-lto

	cmake_src_prepare

	pushd deps/json11 &> /dev/null || die
		eapply "${FILESDIR}/json11-1.0.0-include-cstdint.patch"
	popd &> /dev/null || die
}

src_configure() {
	local libdir=$(get_libdir)
	local mycmakeargs=(
		$(usev browser -DCEF_ROOT_DIR=../${CEF_DIR})
		-DCALM_DEPRECATION=ON
		-DCCACHE_SUPPORT=OFF
		-DENABLE_ALSA=$(usex alsa)
		-DENABLE_SERVICE_UPDATES=OFF
		-DENABLE_AJA=OFF
		-DENABLE_BROWSER=$(usex browser)
		-DENABLE_DECKLINK=$(usex decklink)
		-DENABLE_FREETYPE=$(usex truetype)
		-DENABLE_JACK=$(usex jack)
		-DENABLE_LIBFDK=$(usex fdk)
		-DENABLE_NATIVE_NVENC=OFF
		-DENABLE_NVENC=OFF
		-DENABLE_NEW_MPEGTS_OUTPUT=$(usex mpegts)
		-DENABLE_PIPEWIRE=$(usex pipewire)
		-DENABLE_PULSEAUDIO=$(usex pulseaudio)
		-DENABLE_QSV11=$(usex qsv)
		-DENABLE_RNNOISE=ON
		-DENABLE_RTMPS=$(usex ssl ON OFF) # Needed for bug 880861
		-DENABLE_SPEEXDSP=$(usex speex)
		-DENABLE_UNIT_TESTS=$(usex test)
		-DENABLE_V4L2=$(usex v4l)
		-DENABLE_VLC=$(usex vlc)
		-DENABLE_VST=ON
		-DENABLE_WAYLAND=$(usex wayland)
		-DENABLE_WEBRTC=OFF # Requires libdatachannel.
		-DENABLE_WEBSOCKET=$(usex websocket)
		-DOBS_MULTIARCH_SUFFIX=${libdir#lib}
		-DUNIX_STRUCTURE=1
	)

	if [[ ${PV} != 9999 ]] || ${USE_GIT}; then
		mycmakeargs+=(
			-DOBS_VERSION_OVERRIDE=${PV}
		)
	fi

	if [[ -n "${SWIG4_ROOT}" ]]; then
		mycmakeargs+=(
			-DSWIG_DIR=${SWIG4_ROOT}/usr/share/swig/${SWIG4_PV}/
			-DSWIG_EXECUTABLE=${SWIG4_ROOT}/usr/bin/swig
		)
	fi

	if use lua || use python; then
		mycmakeargs+=(
			-DENABLE_SCRIPTING_LUA=$(usex lua)
			-DENABLE_SCRIPTING_PYTHON=$(usex python)
			-DENABLE_SCRIPTING=ON
		)
	else
		mycmakeargs+=( -DENABLE_SCRIPTING=OFF )
	fi

	if use browser && use ssl; then
		mycmakeargs+=( -DENABLE_WHATSNEW=ON )
	else
		mycmakeargs+=( -DENABLE_WHATSNEW=OFF )
	fi

	CMAKE_PREFIX_PATH=${QT6_ROOT}/lib/cmake \
	PKG_CONFIG_PATH=${QT6_ROOT}/lib/pkgconfig:${FFMPEG_ROOT}/usr/lib64/pkgconfig:$PKG_CONFIG_PATH \
	cmake_src_configure
}

src_install() {
	cmake_src_install

	# external plugins may need some things not installed by default, install them here
	insinto /usr/include/obs/UI/obs-frontend-api
	doins UI/obs-frontend-api/obs-frontend-api.h
}

pkg_postinst() {
	xdg_pkg_postinst

	if ! use alsa && ! use pulseaudio; then
		elog
		elog "For the audio capture features to be available,"
		elog "at least one of the 'alsa' or 'pulseaudio' USE-flags needs to"
		elog "be enabled."
		elog
	fi

	if use v4l && has_version media-video/v4l2loopback; then
		elog
		elog "Depending on system configuration, the v4l2loopback kernel module"
		elog "may need to be loaded manually, and needs to be re-built after"
		elog "kernel changes."
		elog
	fi

	optfeature "VA-API hardware encoding" media-video/ffmpeg[vaapi]
	optfeature "virtual camera support" media-video/v4l2loopback
}

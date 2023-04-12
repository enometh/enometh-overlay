# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Feb 01 15:38:52 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2023 Madhu.  All Rights Reserved.
#
# ;madhu 190317 chawla openjpeg ;madhu 190320 jpeg2k
# ;madhu 190406 2.24.0 clang. turn off the -g 18G freespace check in pkg_prepare
# ;madhu 190423 - removes jit 2.24.1
# ;madhu 190522 - 2.24.2 use clang
# ;madhu 190715 - 2.24.3 back to gcc with env
# ;madhu 190705 - 2.25.2 IUSE wperender - (build uses woff,gcc)
# ;madhu 190801 - 2.25.3
# ;madhu 190820 - 2.25.4 - no Git
# ;madhu 190912 - 2.26.0
# ;madhu 190921 - 2.26.0 - disable_media_source
# ;madhu 191016 - CHAWLA - from git
# ;madhu 191017 - 2.26.1
# ;madhu 200105 - 2.27.1 -> 2.27.3
# ;madhu 200110 - 2.27.4
# ;madhu 200213 - 2.27.90
# ;madhu 200303 - 2.27.91
# ;madhu 201015 - 2.30.1 from git
# ;madhu 210117 - 2.31.1
# ;madhu 211103 - 2.34.1 webkit2gtk-4.1 new slot
# ;madhu 211208 - 2.34.1 soup3 webdriver
# ;madhu 220529 - 2.36.3 gtk4, fix egl on mesa, -seccomp via etc/profile/package.use.force
# ;madhu 221112 - 2.38.1-r410 - revert ebuild to gentoo (soup3 -gtk4 -soup2)
# ;madhu 230412 - 2.40.1-r410

EAPI=8
PYTHON_REQ_USE="xml(+)"
PYTHON_COMPAT=( python3_{8..11} )
USE_RUBY="ruby26 ruby27 ruby30 ruby31"

#CMAKE_BUILD_TYPE=RelWithDebInfo
#CMAKE_BUILD_TYPE=Release

inherit check-reqs flag-o-matic gnome2 optfeature python-any-r1 ruby-single toolchain-funcs cmake

MY_P="webkitgtk-${PV}"
DESCRIPTION="Open source web browser engine"
HOMEPAGE="https://www.webkitgtk.org"
if ${USE_GIT} ; then
	inherit git-r3
	# set this up up manually: GitHub repo downloads gigabytes
	EGIT_REPO_URI="https://example.com/git/webkit.git"
	EGIT_MIRROR_URI="file:///build/git-mirror"
	EGIT_BRANCH="tmp-2.40.0"
	EGIT_CHECKOUT_DIR="${WORKDIR}/${P}"
	EGIT_CLONE_TYPE="shallow"
	EGIT_SUBMODULES=()
	SRC_URI=""
else
SRC_URI="https://www.webkitgtk.org/releases/${MY_P}.tar.xz"
fi

LICENSE="LGPL-2+ BSD"
SLOT="4.1/0" # soname version of libwebkit2gtk-4.1
KEYWORDS="~amd64 ~arm ~arm64 ~ppc ~ppc64 ~riscv ~sparc ~x86"

IUSE="aqua avif +egl examples gamepad geolocation  gles2-only gnome-keyring +gstreamer gtk-doc +introspection pdf +jpeg2k +jumbo-build lcms seccomp spell systemd test wayland X woff webp hyphen elogind webdriver webrtc jpegxl"

# gstreamer with opengl/gles2 needs egl
REQUIRED_USE="
	gles2-only? ( egl )
	gstreamer? ( egl )
	wayland? ( egl )
	|| ( aqua wayland X )
	?? ( elogind systemd )
"

# Tests do not run when built from tarballs
# https://bugs.webkit.org/show_bug.cgi?id=215986
RESTRICT="test"

RDEPEND="
	>=x11-libs/cairo-1.16.0[X?]
	>=media-libs/fontconfig-2.13.0:1.0
	>=media-libs/freetype-2.9.0:2
	>=dev-libs/libgcrypt-1.7.0:0=
	>=x11-libs/gtk+-3.22.0:3[aqua?,introspection?,wayland?,X?]
	>=media-libs/harfbuzz-1.4.2:=[icu(+)]
	>=dev-libs/icu-61.2:=
	media-libs/libjpeg-turbo:0=
	>=media-libs/libepoxy-1.4.0
	>=net-libs/libsoup-3.0.8:3.0[introspection?]
	>=dev-libs/libxml2-2.8.0:2
	>=media-libs/libpng-1.4:0=
	dev-db/sqlite:3
	sys-libs/zlib:0
	>=app-accessibility/at-spi2-core-2.46.0:2
	webp? ( media-libs/libwebp:= )

	>=dev-libs/glib-2.70.0:2
	>=dev-libs/libxslt-1.1.7
	woff? ( media-libs/woff2 )
	gnome-keyring? ( app-crypt/libsecret )
	introspection? ( >=dev-libs/gobject-introspection-1.59.1:= )
	dev-libs/libtasn1:=
	spell? ( >=app-text/enchant-0.22:2 )
	gstreamer? (
		>=media-libs/gstreamer-1.20:1.0
		>=media-libs/gst-plugins-base-1.20:1.0[egl?,X?]
		gles2-only? ( media-libs/gst-plugins-base:1.0[gles2] )
		!gles2-only? ( media-libs/gst-plugins-base:1.0[opengl] )
		>=media-plugins/gst-plugins-opus-1.20:1.0
		>=media-libs/gst-plugins-bad-1.20:1.0
	)

	X? (
		x11-libs/libX11
		x11-libs/libXcomposite
		x11-libs/libXdamage
		x11-libs/libXrender
		x11-libs/libXt
	)

	hyphen? ( dev-libs/hyphen )
	jpeg2k? ( >=media-libs/openjpeg-2.2.0:2= )
	avif? ( >=media-libs/libavif-0.9.0:= )
	jpegxl? ( media-libs/libjxl:= )
	lcms? ( media-libs/lcms:2 )

	egl? ( media-libs/mesa[egl(+)] )
	gles2-only? ( media-libs/mesa[gles2] )
	!gles2-only? ( virtual/opengl )
	wayland? (
		dev-libs/wayland
		>=dev-libs/wayland-protocols-1.12
		>=gui-libs/libwpe-1.5.0:1.0
		>=gui-libs/wpebackend-fdo-1.7.0:1.0
	)

	seccomp? (
		>=sys-apps/bubblewrap-0.3.1
		sys-libs/libseccomp
		sys-apps/xdg-dbus-proxy
	)

	systemd? (  sys-apps/systemd:= )
	elogind? (  sys-auth/elogind:= )
	gamepad? ( >=dev-libs/libmanette-0.2.4 )

	webdriver? ( !net-libs/webkit-gtk-webdriver )
	!webdriver? ( net-libs/webkit-gtk-webdriver )
"
DEPEND="${RDEPEND}"
# Need real bison, not yacc
BDEPEND="
	${PYTHON_DEPS}
	${RUBY_DEPS}
	>=app-accessibility/at-spi2-core-2.5.3
	dev-util/gdbus-codegen
	dev-util/glib-utils
	>=dev-util/gperf-3.0.1
	dev-util/unifdef
	>=sys-devel/bison-2.4.3
	|| ( >=sys-devel/gcc-7.3 >=sys-devel/clang-5 )
	sys-devel/gettext
	virtual/pkgconfig

	>=dev-lang/perl-5.10
	virtual/perl-Data-Dumper
	virtual/perl-Carp
	virtual/perl-JSON-PP

	gtk-doc? ( dev-util/gi-docgen )
	geolocation?  ( >=app-misc/geoclue-2.1.5:2.0 )
"

#S="${WORKDIR}/${MY_P}"

CHECKREQS_DISK_BUILD="18G" # and even this might not be enough, bug #417307

pkg_pretend() {
	if [[ ${MERGE_TYPE} != "binary" ]] ; then
		if is-flagq "-g*" && ! is-flagq "-g*0" ; then
			einfo "Checking for sufficient disk space to build ${PN} with debugging CFLAGS"
			check-reqs_pkg_pretend
		fi

		if ! test-flag-CXX -std=c++17 ; then
			die "You need at least GCC 7.3.x or Clang >= 5 for C++17-specific compiler flags"
		fi
	fi
}

pkg_setup() {
	if [[ ${MERGE_TYPE} != "binary" ]] && is-flagq "-g*" && ! is-flagq "-g*0" ; then
		check-reqs_pkg_setup
	fi

	python-any-r1_pkg_setup
}

src_prepare() {
	cmake_src_prepare
	gnome2_src_prepare
}

src_configure() {
	# Respect CC, otherwise fails on prefix #395875
	tc-export CC

	# It does not compile on alpha without this in LDFLAGS
	# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=648761
	use alpha && append-ldflags "-Wl,--no-relax"

	# ld segfaults on ia64 with LDFLAGS --as-needed, bug #555504
	use ia64 && append-ldflags "-Wl,--no-as-needed"

	# Sigbuses on SPARC with mcpu and co., bug #???
	use sparc && filter-flags "-mvis"

	# https://bugs.webkit.org/show_bug.cgi?id=42070 , #301634
	use ppc64 && append-flags "-mminimal-toc"

	# Try to use less memory, bug #469942 (see Fedora .spec for reference)
	# --no-keep-memory doesn't work on ia64, bug #502492
	if ! use ia64; then
		append-ldflags $(test-flags-CCLD "-Wl,--no-keep-memory")
	fi

	# Ruby situation is a bit complicated. See bug 513888
	local rubyimpl
	local ruby_interpreter=""
	for rubyimpl in ${USE_RUBY}; do
		if has_version -b "virtual/rubygems[ruby_targets_${rubyimpl}]"; then
			ruby_interpreter="-DRUBY_EXECUTABLE=$(type -P ${rubyimpl})"
		fi
	done
	# This will rarely occur. Only a couple of corner cases could lead us to
	# that failure. See bug 513888
	[[ -z $ruby_interpreter ]] && die "No suitable ruby interpreter found"

	# TODO: Check Web Audio support
	# should somehow let user select between them?

	local mycmakeargs=(
		-DPython_EXECUTABLE="${PYTHON}"
		${ruby_interpreter}
		$(cmake_use_find_package gles2-only OpenGLES2)
		$(cmake_use_find_package egl EGL)
		$(cmake_use_find_package !gles2-only OpenGL)
		-DBWRAP_EXECUTABLE:FILEPATH="${EPREFIX}"/usr/bin/bwrap # If bubblewrap[suid] then portage makes it go-r and cmake find_program fails with that
		-DDBUS_PROXY_EXECUTABLE:FILEPATH="${EPREFIX}"/usr/bin/xdg-dbus-proxy
		-DPORT=GTK
		# Source/cmake/WebKitFeatures.cmake
		-DENABLE_API_TESTS=$(usex test)
		-DENABLE_BUBBLEWRAP_SANDBOX=$(usex seccomp)
		-DENABLE_GAMEPAD=$(usex gamepad)
		-DENABLE_MINIBROWSER=$(usex examples)
		-DENABLE_PDFJS=$(usex pdf)
		-DENABLE_GEOLOCATION=$(usex geolocation) # Runtime optional (talks over dbus service)
		-DENABLE_SPELLCHECK=$(usex spell)
		-DENABLE_UNIFIED_BUILDS=$(usex jumbo-build)
		-DENABLE_VIDEO=$(usex gstreamer)
		-DUSE_GSTREAMER_WEBRTC=$(usex gstreamer)
		-DENABLE_WEB_RTC=$(usex webrtc)
		-DUSE_GSTREAMER_TRANSCODER=$(usex gstreamer)
		-DENABLE_WEBGL=ON
		-DENABLE_WEB_AUDIO=$(usex gstreamer)
		# Source/cmake/OptionsGTK.cmake
		-DENABLE_GLES2=$(usex gles2-only)
		-DENABLE_DOCUMENTATION=OFF #$(usex gtk-doc)
		-DENABLE_INTROSPECTION=$(usex introspection)
		-DENABLE_JOURNALD_LOG=$(usex systemd || usex elogind)
		-DENABLE_QUARTZ_TARGET=$(usex aqua)
		-DENABLE_WAYLAND_TARGET=$(usex wayland)
		-DENABLE_X11_TARGET=$(usex X)
		-DUSE_GBM=ON
		-DUSE_AVIF=$(usex avif)
		-DUSE_GTK4=OFF # $(usex gtk4)
		-DENABLE_WEBDRIVER=$(usex webdriver)
		-DUSE_JPEGXL=$(usex jpegxl)
		-DUSE_LCMS=$(usex lcms)
		-DUSE_LIBHYPHEN=$(usex hyphen)
		-DUSE_LIBSECRET=$(usex gnome-keyring)
		-DUSE_OPENGL_OR_ES=ON
		-DUSE_OPENJPEG=$(usex jpeg2k)
		-DUSE_SOUP2=OFF # $(usex soup3 no yes)
		-DUSE_WOFF2=$(usex woff)
		-DUSE_WPE_RENDERER=$(usex wayland) # WPE renderer is used to implement accelerated compositing under wayland

		-DENABLE_MEDIA_CAPTURE:BOOL=ON
		-DENABLE_MEDIA_STREAM:BOOL=ON
		-DENABLE_MEDIA_SESSION:BOOL=ON
		-DENABLE_MEDIA_RECORDER:BOOL=ON
		-DENABLE_SERVICE_WORKER:BOOL=ON
		-DENABLE_RELEASE_LOG:BOOL=OFF

		-DUSE_ANGLE_WEBGL=OFF #XXX
		-DLOG_DISABLED:BOOL=FALSE
		-DASSERTS_ARE_WARNINGS:BOOL=ON #madhu 200303
		-DLOG_ENABLED:BOOL=ON
		-DENABLE_RELEASE_LOG:BOOL=OFF
		-DENABLE_PERIODIC_MEMORY_MONITOR:BOOL=OFF
		-DENABLE_MEDIA_STATISTICS:BOOL=ON
		-DENABLE_NETWORK_CACHE_SPECULATIVE_REVALIDATION:BOOL=OFF
		-DENABLE_SERVER_PRECONNECT:BOOL=OFF
	)

	# ;madhu 210117 cmake.eclass stopped adding -NDEBUG
	# https://gitweb.gentoo.org/repo/gentoo.git/commit/?id=95577dd5076a8e9864e82879fd3af97cf63fcfe9
	# https://bugs.gentoo.org/761238
	if [[ ${CMAKE_BUILD_TYPE} = Gentoo ]]; then
		if ! in_iuse debug || ! use debug; then
			local CPPFLAGS=${CPPFLAGS}
			append-cppflags -DNDEBUG
		fi
	fi

#	WK_USE_CCACHE=NO
	cmake_src_configure
}

src_install() {
	cmake_src_install

	if ! use webdriver; then
		rm -fv ${ED}/usr/bin/WebKitWebDriver
	fi

	insinto /usr/share/gtk-doc/html
	# This will install API docs specific to webkit2gtk-4.1
	doins -r "${S}"/Documentation/{jsc-glib,webkit2gtk,webkit2gtk-web-extension}-${SLOT%/*}
}

pkg_postinst() {
	optfeature "geolocation service (used at runtime if available)" "app-misc/geoclue"
	optfeature "Common Multimedia codecs" "media-plugins/gst-plugins-meta"
	optfeature "(MPEG-)DASH support" "media-plugins/gst-plugins-dash"
	optfeature "HTTP-Live-Streaming support" "media-plugins/gst-plugins-hls"
}

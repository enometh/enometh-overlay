# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Feb 01 15:38:52 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2022 Madhu.  All Rights Reserved.
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
# ;madhu 221112 - 2.38.2 gtk4 slot 4.1
# ;madhu 240321 - 2.44.0-r600 gtk4 slot 6.0 no jpeg2k, FAKEBUILD! (see note below), USE_GIT, -Dexport_compile_commands etc. need patches that are in git or have to be applied by hand. gtk-doc builds documentation, doc installs shipped documentation if it exists.

EAPI=8
PYTHON_REQ_USE="xml(+)"
PYTHON_COMPAT=( python3_{9..12} )
USE_RUBY="ruby26 ruby30 ruby31 ruby32"

USE_GIT=false
FAKEBUILD=true

# FAKEBUILD skips cmake_src_compile and cmake_src_install, but copies
# /dev/shm/gtk-root/ if it exists to ${ED}. i.e. we assume the packager
# has called `ninja' and `DESTDIR=/dev/shm/gtk-root ninja install' by
# hand.

#CMAKE_BUILD_TYPE=RelWithDebInfo
CMAKE_BUILD_TYPE=Release

inherit check-reqs flag-o-matic gnome2 optfeature python-any-r1 ruby-single toolchain-funcs cmake

MY_P="webkitgtk-${PV}"
DESCRIPTION="Open source web browser engine"
HOMEPAGE="https://www.webkitgtk.org"
if ${USE_GIT} ; then
	inherit git-r3
	# set this up up manually: GitHub repo downloads gigabytes
	EGIT_REPO_URI="https://example.com/git/webkit.git"
	EGIT_MIRROR_URI="file:///build/git-mirror"
	EGIT_BRANCH="tmp-2.44.0"
	EGIT_CHECKOUT_DIR="${WORKDIR}/${P}"
	EGIT_CLONE_TYPE="shallow"
	EGIT_SUBMODULES=()
	SRC_URI=""
else
SRC_URI="https://www.webkitgtk.org/releases/${MY_P}.tar.xz"
fi

LICENSE="LGPL-2+ BSD"
SLOT="6/0" # soname version of libwebkit2gtk-6.0
KEYWORDS="amd64 arm arm64 ~loong ppc ppc64 ~riscv ~sparc x86"

IUSE="aqua avif examples gamepad keyring +gstreamer +introspection pdf jpegxl +jumbo-build lcms seccomp spell systemd wayland X test gtk-doc geolocation webp woff webdriver hyphen elogind webrtc doc"
REQUIRED_USE="|| ( aqua wayland X )"

# Tests do not run when built from tarballs
# https://bugs.webkit.org/show_bug.cgi?id=215986
RESTRICT="test"

# Dependencies found at Source/cmake/OptionsGTK.cmake
# Missing WebRTC support, but ENABLE_MEDIA_STREAM/ENABLE_WEB_RTC is
# experimental upstream (PRIVATE OFF) and shouldn't be used yet in 2.30
# >=gst-plugins-opus-1.14.4-r1 for opusparse (required by MSE)
# TODO: gst-plugins-base[X] is only needed when build configuration ends up
# with GLX set, but that's a bit automagic too to fix
# Softblocking webkit-gtk-2.38:4 as we going to use webkit-2.38:4.1's WebKitDriver binary

# 	media-libs/libglvnd
# 	!<net-libs/webkit-gtk-2.38:4
RDEPEND="
	>=x11-libs/cairo-1.16.0[X?]
	>=media-libs/fontconfig-2.13.0:1.0
	>=media-libs/freetype-2.9.0:2
	>=dev-libs/libgcrypt-1.7.0:0=
	>=x11-libs/gtk+-3.22.0:3[aqua?,introspection?,wayland?,X?]
	>=gui-libs/gtk-4.4.0:4[introspection?]
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
	keyring? ( app-crypt/libsecret )
	introspection? ( >=dev-libs/gobject-introspection-1.59.1:= )
	dev-libs/libtasn1:=
	spell? ( >=app-text/enchant-0.22:2 )
	gstreamer? (
		>=media-libs/gstreamer-1.20:1.0
		>=media-libs/gst-plugins-base-1.20:1.0[egl,X?]
		media-libs/gst-plugins-base:1.0[opengl]
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
	jpegxl? ( >=media-libs/libjxl-0.7.0 )
	avif? ( >=media-libs/libavif-0.9.0:= )
	lcms? ( media-libs/lcms:2 )

	media-libs/mesa
	wayland? (
		>=dev-libs/wayland-1.15
		>=dev-libs/wayland-protocols-1.15
		>=gui-libs/libwpe-1.5.0:1.0
		>=gui-libs/wpebackend-fdo-1.7.0:1.0
	)

	seccomp? (
		>=sys-apps/bubblewrap-0.3.1
		sys-libs/libseccomp
		sys-apps/xdg-dbus-proxy
	)

	systemd? ( sys-apps/systemd:= )
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
"

if ! ${USE_GIT} ; then
	S="${WORKDIR}/${MY_P}"
fi

CHECKREQS_DISK_BUILD="18G" # and even this might not be enough, bug #417307

# We cannot use PATCHES because src_prepare() calls cmake_src_prepare and
# gnome2_src_prepare, and both apply ${PATCHES[@]}
PATCHES=()

pkg_pretend() {
	if [[ ${MERGE_TYPE} != "binary" ]] ; then
		if is-flagq "-g*" && ! is-flagq "-g*0" ; then
			einfo "Checking for sufficient disk space to build ${PN} with debugging CFLAGS"
			check-reqs_pkg_pretend
		fi

		if ! test-flag-CXX -std=c++20 ; then
			die "You need at least GCC 13 or Clang-14 for C++20-specific compiler flags"
		fi
		# ;madhu 240321 - if building with clang-14,
		# WebKitCompilerFlags.cmake has to be patched to use
		# -stdlib=libc++ (as libstdc++ which it buggily uses by default
		# fail)
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

	# Fix USE=-jumbo-build compilation on arm64
#	eapply "${FILESDIR}"/2.42.1-arm64-non-jumbo-fix.patch
}

src_configure() {
	# Respect CC, otherwise fails on prefix #395875
	tc-export CC

	# ODR violations (bug #915230, https://bugs.webkit.org/show_bug.cgi?id=233007)
	filter-lto

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
	local RUBY
	for rubyimpl in ${USE_RUBY}; do
		if has_version -b "virtual/rubygems[ruby_targets_${rubyimpl}(-)]"; then
			RUBY="$(type -P ${rubyimpl})"
			ruby_interpreter="-DRUBY_EXECUTABLE=${RUBY}"
		fi
	done
	# This will rarely occur. Only a couple of corner cases could lead us to
	# that failure. See bug 513888
	[[ -z ${ruby_interpreter} ]] && die "No suitable ruby interpreter found"
	# JavaScriptCore/Scripts/postprocess-asm invokes another Ruby script directly
	# so it doesn't respect RUBY_EXECUTABLE, bug #771744.
	sed -i -e "s:#!/usr/bin/env ruby:#!${RUBY}:" $(grep -rl "/usr/bin/env ruby" Source/JavaScriptCore || die) || die

	# TODO: Check Web Audio support
	# should somehow let user select between them?

	local mycmakeargs=(
		-DPython_EXECUTABLE="${PYTHON}"
		${ruby_interpreter}
		# If bubblewrap[suid] then portage makes it go-r and cmake find_program fails with that
		-DBWRAP_EXECUTABLE:FILEPATH="${EPREFIX}"/usr/bin/bwrap
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
		-DENABLE_WEBDRIVER=$(usex webdriver) # Disable WebDriver for webkit2gtk-6.0 and use the webkit2gtk-4.1
		-DENABLE_WEBGL=ON
		-DENABLE_WEB_AUDIO=$(usex gstreamer)
		-DUSE_AVIF=$(usex avif)
		# Source/cmake/OptionsGTK.cmake
		-DENABLE_DOCUMENTATION=$(usex gtk-doc)
		-DENABLE_INTROSPECTION=$(usex introspection)
		-DENABLE_JOURNALD_LOG=$(usex systemd || usex elogind)
		-DENABLE_QUARTZ_TARGET=$(usex aqua)
		-DENABLE_WAYLAND_TARGET=$(usex wayland)
		-DENABLE_X11_TARGET=$(usex X)
		-DUSE_GBM=ON
		-DUSE_GTK4=ON # webkit2gtk-6.0
		-DUSE_JPEGXL=$(usex jpegxl)
		-DUSE_LCMS=$(usex lcms)
		-DUSE_LIBHYPHEN=$(usex hyphen)
		-DUSE_LIBSECRET=$(usex keyring)
		-DUSE_OPENGL_OR_ES=ON
		-DUSE_SOUP2=OFF
		-DUSE_WOFF2=$(usex woff)

		-DASSERTS_ARE_WARNINGS:BOOL=ON #madhu 200303
		-DCMAKE_EXPORT_COMPILE_COMMANDS=ON
		-DENABLE_MEDIA_STATISTICS:BOOL=ON
		-DENABLE_NETWORK_CACHE_SPECULATIVE_REVALIDATION:BOOL=OFF
		-DENABLE_PERIODIC_MEMORY_MONITOR:BOOL=OFF
		-DENABLE_RELEASE_LOG:BOOL=OFF
		-DENABLE_RELEASE_LOG:BOOL=ON
		-DENABLE_SERVER_PRECONNECT:BOOL=OFF
		-DLOG_DISABLED:BOOL=FALSE
		-DLOG_ENABLED:BOOL=ON
	)

	# https://bugs.gentoo.org/761238
#	append-cppflags -DNDEBUG
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

src_compile() {
	if ${FAKEBUILD}; then
		ewarn "FAKEBUILD skip SRC COMPILE"
		return;
	fi
	cmake_src_compile
}

src_install() {
	if ${FAKEBUILD}; then
		ewarn "FAKEBUILD skip SRC INSTALL"
		UNPACK_DIR=/dev/shm/gtk-root
		if [ -d ${UNPACK_DIR}  ]; then
			rsync -i -avzHOJX --delete ${UNPACK_DIR}/ ${ED}/
		fi
	else
		cmake_src_install
	fi
	docpkgs="javascriptcoregtk-6.0 webkitgtk-6.0 webkitgtk-web-process-extension-6.0"
	if use gtk-doc || use doc; then
		mkdir -pv ${ED}/usr/share/gtk-doc/html
		if use gtk-doc; then
			for i in ${docpkgs} ; do
				mv ${ED}/usr/share/doc/$i ${ED}/usr/share/gtk-doc/html/$i
			done
		elif use doc && -d ${S}/Documentation/ ; then
			for i in ${docpkgs}; do	# FIXME use of rsyncd without declaring it
				rsync -avH ${S}/Documentation/$i/ ${ED}/usr/share/gtk-doc/html/$i
			done
		fi
	fi
	# Don't ship anything under /usr/share/doc
	for i in ${docpkgs}; do
		d=${ED}/usr/share/doc/$i
		[ -d  ${d} ] && rm -rf ${d}
	done
}

pkg_postinst() {
	optfeature "geolocation service (used at runtime if available)" "app-misc/geoclue"
	optfeature "Common Multimedia codecs" "media-plugins/gst-plugins-meta"
	optfeature "(MPEG-)DASH support" "media-plugins/gst-plugins-dash"
	optfeature "HTTP-Live-Streaming support" "media-plugins/gst-plugins-hls"
}

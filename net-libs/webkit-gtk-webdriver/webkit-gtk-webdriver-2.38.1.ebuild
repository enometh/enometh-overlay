# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Feb 01 15:38:52 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2022 Madhu.  All Rights Reserved.
#

# ;madhu 211104 - 2.34.1 net-libs/webkitgk-webdriver - split out into a
# separate package. Add IUSE=webdriver for net-libs/webkit-gtk without
# which the binary is deleted from that build. This ebuild is a copy of
# the webkit-gtk ebuild except USE is expected to be empty, and it just
# builds the webdriver binary and copies the webdriver binary to
# /usr/bin. TODO: IUSE flags are bogus.  correct dependency declaration:
# conflict with net-libs/webkit-gtk[webdriver] only if it exists.  maybe
# extract common parts into an eclass
#
# ;madhu 221024 2.38.1 -- WEIRD if cmake is at the end of the inherit
# line, then interrupting and running ebuild prepare/compile a second
# time results in a die _PYTHON_UNSUPPORTED_IMPLS integrity check
# failed. patch ruby-utils.eclass RUBY_TARGETS_PREFERENCE+=" ruby26" to
# keep using ruby26.


EAPI=8
PYTHON_REQ_USE="xml(+)"
PYTHON_COMPAT=( python3_{8..11} )
USE_RUBY="ruby26 ruby27 ruby30"
CMAKE_BUILD_TYPE=Release

inherit check-reqs cmake flag-o-matic gnome2  python-any-r1 ruby-single toolchain-funcs

MY_P="webkitgtk-${PV}"
DESCRIPTION="Webdriver for the WebKit Open source web browser engine"
HOMEPAGE="https://www.webkitgtk.org"
if ${USE_GIT} ; then
	inherit git-r3
	# set this up up manually: GitHub repo downloads gigabytes
	EGIT_REPO_URI="https://example.com/git/webkit.git"
	EGIT_MIRROR_URI="file:///build/git-mirror"
	EGIT_BRANCH="tmp-2.38.1"
	EGIT_CHECKOUT_DIR="${WORKDIR}/${P}"
	EGIT_CLONE_TYPE="shallow"
	EGIT_SUBMODULES=()
	SRC_URI=""
else
	SRC_URI="https://www.webkitgtk.org/releases/${MY_P}.tar.xz"
fi

LICENSE="LGPL-2+ BSD"
SLOT="5/37" # soname version of libwebkit2gtk-4.0
KEYWORDS="amd64 arm arm64 ~ppc ppc64 ~riscv ~sparc x86"

IUSE="jumbo-build"

# Tests fail to link for inexplicable reasons
# https://bugs.webkit.org/show_bug.cgi?id=148210
RESTRICT="test"

RDEPEND="
	>=x11-libs/cairo-1.16.0
	>=media-libs/fontconfig-2.13.0:1.0
	>=media-libs/freetype-2.9.0:2
	>=x11-libs/gtk+-3.22.0:3
	gui-libs/gtk:4
	>=media-libs/harfbuzz-1.4.2:=[icu(+)]
	media-libs/libjpeg-turbo:0=
	>=dev-libs/icu-61.2:=
	>=net-libs/libsoup-3.0.8:3.0
	>=dev-libs/glib-2.67.1:2
	>=dev-libs/libxslt-1.1.7
	>=dev-libs/atk-2.16.0
	media-libs/libwebp:=
	media-libs/mesa

	!net-libs/webkit-gtk[webdriver]
"
DEPEND="${RDEPEND}"
BDEPEND="
	${PYTHON_DEPS}
	${RUBY_DEPS}
	dev-util/glib-utils
	>=dev-util/gperf-3.0.1
	>=sys-devel/bison-2.4.3
	|| ( >=sys-devel/gcc-7.3 >=sys-devel/clang-5 )
	sys-devel/gettext
	virtual/pkgconfig

	>=dev-lang/perl-5.10
	virtual/perl-Data-Dumper
	virtual/perl-Carp
	virtual/perl-JSON-PP
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

	# We try to use gold when possible for this package
#	if ! tc-ld-is-gold ; then
#		append-ldflags "-Wl,--reduce-memory-overheads"
#	fi

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


	local mycmakeargs=(
		${ruby_interpreter}
		-DENABLE_UNIFIED_BUILDS=$(usex jumbo-build)
		-DENABLE_QUARTZ_TARGET=OFF
		-DENABLE_API_TESTS=OFF
		-DENABLE_GTKDOC=OFF
		-DENABLE_MINIBROWSER:BOOL=OFF
		-DUSE_LIBHYPHEN=OFF
		-DENABLE_GEOLOCATION=OFF
		-DCMAKE_DISABLE_FIND_PACKAGE_GLES2=ON
		-DENABLE_GLES2=OFF
		-DENABLE_VIDEO=OFF
		-DENABLE_WEB_AUDIO=OFF
		-DENABLE_WEBP=OFF
		-DENABLE_INTROSPECTION=OFF
		-DUSE_LIBNOTIFY=OFF
		-DUSE_LIBSECRET=OFF
		-DUSE_OPENJPEG=OFF
		-DUSE_WOFF2=OFF
		-DENABLE_SPELLCHECK=OFF
		-DUSE_SYSTEMD=OFF
		-DENABLE_GAMEPAD=OFF
		-DENABLE_WAYLAND_TARGET=OFF
		-DUSE_WPE_RENDERER=OFF
		-DCMAKE_DISABLE_FIND_PACKAGE_EGL=ON
		-DENABLE_WEBDRIVER=ON
		-DCMAKE_DISABLE_FIND_PACKAGE_OPENGL=ON
		-DENABLE_X11_TARGET=OFF
		-DENABLE_WEBGL=OFF
		-DENABLE_BUBBLEWRAP_SANDBOX=OFF
		-DASSERTS_ARE_WARNINGS:BOOL=ON #madhu 200303
		-DLOG_ENABLED:BOOL=ON
		-DUSE_OPENGL_OR_ES=OFF
		-DENABLE_WEB_CRYPTO=OFF
		-DUSE_LCMS=OFF
		-DPORT=GTK
		${ruby_interpreter}
	)
	# ;madhu 210117 cmake.eclass stopped adding -NDEBUG
	# https://gitweb.gentoo.org/repo/gentoo.git/commit/?id=95577dd5076a8e9864e82879fd3af97cf63fcfe9
	if [[ ${CMAKE_BUILD_TYPE} = Gentoo ]]; then
		if ! in_iuse debug || ! use debug; then
			local CPPFLAGS=${CPPFLAGS}
			append-cppflags -DNDEBUG
		fi
	fi

	# WK_USE_CCACHE=NO
	cmake_src_configure
}

src_compile() {
	ninja -C ${BUILD_DIR} WebKitWebDriver || die
}


src_install() {
	dobin ${BUILD_DIR}/bin/WebKitWebDriver || die
}

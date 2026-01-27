# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sat Apr 18 15:00:13 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200418 68.6.1 -> 68.0
# ;madhu 200426 68.6.0
# in.archive.ubuntu.com/ubuntu/pool/main/m/mozjs68/mozjs68_68.6.0.orig.tar.xz
# drop firefox patches but keep our spidermonkey patches
# ;madhu 210116 78.6.0 -> 78.6.1
# ;madhu 210329 78.9.0
# ;madhu 220131 91.5.0 -> 91.0.2
# ;madhu 220412 91.7.1 -> 91.8.0
# ;madhu 220130 91.5.0.ebuild -> 96.3.0 (bogus build)
# ;madhu 220927 102.3.0
# ;madhu 260126 140.7.0 stripped down from gentoo. use /opt/rust-bin-1.92.0/ /opt/llvm-21.1.8/, ignore all lto

EAPI="8"

USE_GIT=false
NOPATCHES=true
FAKEBUILD=false

LLVMVER=2.1.18
RUSTVER=1.92.0
LLVMROOT=${EPREFIX}/opt/llvm-${LLVMVER}
RUSTROOT=${EPREFIX}/opt/rust-bin-${RUSTVER}

# Patch version
FIREFOX_PATCHSET="firefox-140esr-patches-03.tar.xz"
SPIDERMONKEY_PATCHSET="spidermonkey-140-patches-02.tar.xz"

LLVM_MAX_SLOT=21
LLVM_COMPAT=( 19 20 21 )
RUST_NEEDS_LLVM=1
RUST_MIN_VER=1.82.0

PYTHON_COMPAT=( python3_{11..14} )
PYTHON_REQ_USE="ncurses,ssl,xml(+)"

WANT_AUTOCONF="2.1"

inherit check-reqs flag-o-matic llvm-r1 multiprocessing python-any-r1 rust toolchain-funcs

MY_PN="mozjs"
MY_PV="${PV/_pre*}" # Handle Gentoo pre-releases

MY_MAJOR=$(ver_cut 1)

MOZ_ESR=yes

MOZ_PV=${PV}
MOZ_PV_SUFFIX=
if [[ ${PV} =~ (_(alpha|beta|rc).*)$ ]] ; then
	MOZ_PV_SUFFIX=${BASH_REMATCH[1]}

	# Convert the ebuild version to the upstream Mozilla version
	MOZ_PV="${MOZ_PV/_alpha/a}" # Handle alpha for SRC_URI
	MOZ_PV="${MOZ_PV/_beta/b}"  # Handle beta for SRC_URI
	MOZ_PV="${MOZ_PV%%_rc*}"    # Handle rc for SRC_URI
fi

if [[ -n ${MOZ_ESR} ]] ; then
	# ESR releases have slightly different version numbers
	MOZ_PV="${MOZ_PV}esr"
fi

MOZ_PN="firefox"
MOZ_P="${MOZ_PN}-${MOZ_PV}"
MOZ_PV_DISTFILES="${MOZ_PV}${MOZ_PV_SUFFIX}"
MOZ_P_DISTFILES="${MOZ_PN}-${MOZ_PV_DISTFILES}"

MOZ_SRC_BASE_URI="https://archive.mozilla.org/pub/${MOZ_PN}/releases/${MOZ_PV}"

if [[ ${PV} == *_rc* ]] ; then
	MOZ_SRC_BASE_URI="https://archive.mozilla.org/pub/${MOZ_PN}/candidates/${MOZ_PV}-candidates/build${PV##*_rc}"
fi

if ! ${NOPATCHES}; then
PATCH_URIS=(
	https://dev.gentoo.org/~{juippis,whissi}/mozilla/patchsets/${FIREFOX_PATCHSET}
	https://dev.gentoo.org/~{juippis,whissi}/mozilla/patchsets/${SPIDERMONKEY_PATCHSET}
)
fi

if ${USE_GIT}; then
	inherit git-r3
	if ! ${NOPATCHES} ; then
	SRC_URI="${PATCH_URIS[@]}"
	fi
	EGIT_MIRROR_URI="file:///build/git-mirror/"
	EGIT_REPO_URI="https://example.com/git/${MOZ_PN}.git"
	EGIT_BRANCH="release" # no patches
	EGIT_CHECKOUT_DIR="${WORKDIR}/${MOZ_PN}-${MY_PV}"
	EGIT_CLONE_TYPE="shallow"
	EGIT_SUBMODULES=()

	KEYWORDS="amd64 arm64 x86"
else

	SRC_URI="${MOZ_SRC_BASE_URI}/source/${MOZ_P}.source.tar.xz -> ${MOZ_P_DISTFILES}.source.tar.xz
	${PATCH_URIS[@]}"
fi

if ${USE_GIT}; then
#keep the tarball in manifest anyway
SRC_URI+=" ${MOZ_SRC_BASE_URI}/source/${MOZ_P}.source.tar.xz -> ${MOZ_P_DISTFILES}.source.tar.xz"
fi

DESCRIPTION="SpiderMonkey is Mozilla's JavaScript engine written in C and C++"
HOMEPAGE="https://spidermonkey.dev https://firefox-source-docs.mozilla.org/js/index.html "
LICENSE="MPL-2.0"
SLOT="$(ver_cut 1)"
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~ppc ~ppc64 ~riscv ~x86"

IUSE="clang cpu_flags_arm_neon debug +jit lto test"
IUSE+=" system-icu system-nspr"

#RESTRICT="test"
RESTRICT="!test? ( test )"

# handle clang manually
BDEPEND="${PYTHON_DEPS}
	virtual/rust
	virtual/pkgconfig
	test? (
		$(python_gen_any_dep 'dev-python/six[${PYTHON_USEDEP}]')
	)"

# check but use bundled anyway, virtual/zlib
DEPEND="system-icu? ( dev-libs/icu )
	system-nspr? ( dev-libs/nspr )
	sys-libs/readline:0=
	sys-libs/zlib"
RDEPEND="${DEPEND}"

S="${WORKDIR}/firefox-${MY_PV}"

PATCHES=(
	$FILESDIR/140.7--Don-t-die-on-SpiderMonkey-checks.patch
	$FILESDIR/140.7-build-Don-t-fail-when-passing-standardized-autotools.patch
)

llvm_check_deps() { return 1; }

python_check_deps() {
	if use test ; then
		python_has_version "dev-python/six[${PYTHON_USEDEP}]"
	fi
}

mozconfig_add_options_ac() {
	debug-print-function ${FUNCNAME} "$@"

	if [[ ${#} -lt 2 ]] ; then
		die "${FUNCNAME} requires at least two arguments"
	fi

	local reason=${1}
	shift

	local option
	for option in ${@} ; do
		echo "ac_add_options ${option} # ${reason}" >>${MOZCONFIG}
	done
}

mozconfig_add_options_mk() {
	debug-print-function ${FUNCNAME} "$@"

	if [[ ${#} -lt 2 ]] ; then
		die "${FUNCNAME} requires at least two arguments"
	fi

	local reason=${1}
	shift

	local option
	for option in ${@} ; do
		echo "mk_add_options ${option} # ${reason}" >>${MOZCONFIG}
	done
}

mozconfig_use_enable() {
	debug-print-function ${FUNCNAME} "$@"

	if [[ ${#} -lt 1 ]] ; then
		die "${FUNCNAME} requires at least one arguments"
	fi

	local flag=$(use_enable "${@}")
	mozconfig_add_options_ac "$(use ${1} && echo +${1} || echo -${1})" "${flag}"
}

pkg_pretend() {
	if use test ; then
		CHECKREQS_DISK_BUILD="4400M"
	else
		CHECKREQS_DISK_BUILD="4300M"
	fi

	check-reqs_pkg_pretend
}

pkg_setup() {

		if use test ; then
			CHECKREQS_DISK_BUILD="4400M"
		else
			CHECKREQS_DISK_BUILD="4300M"
		fi

		check-reqs_pkg_setup

#		llvm_r1_pkg_setup
#		rust_pkg_setup
		python-any-r1_pkg_setup

		# Build system is using /proc/self/oom_score_adj, bug #604394
		addpredict /proc/self/oom_score_adj

		if ! mountpoint -q /dev/shm ; then
			# If /dev/shm is not available, configure is known to fail with
			# a traceback report referencing /usr/lib/pythonN.N/multiprocessing/synchronize.py
			ewarn "/dev/shm is not mounted -- expect build failures!"
		fi

		# Ensure we use C locale when building, bug #746215
		export LC_ALL=C
}

src_unpack() {
	default
	if ${USE_GIT}; then
		git-r3_src_unpack
	fi
}

src_prepare() {
#	use lto && rm -v "${WORKDIR}"/firefox-patches/*-LTO-Only-enable-LTO-*.patch

	if ! ${NOPATCHES}; then
		eapply "${WORKDIR}"/firefox-patches
		eapply "${WORKDIR}"/spidermonkey-patches
	fi

	default

	# sed-in toolchain prefix
	# FIXME
	sed -i \
		-e "s/objdump/${CHOST}-objdump/" \
		python/mozbuild/mozbuild/configure/check_debug_ranges.py \
		|| warn "sed failed to set toolchain prefix"

	# use prefix shell in wrapper linker scripts, bug #789660
	hprefixify "${S}"/../../build/cargo-{,host-}linker

	einfo "Removing pre-built binaries ..."
	find third_party -type f \( -name '*.so' -o -name '*.o' \) -print -delete # || die

	MOZJS_BUILDDIR="${WORKDIR}/build"
	mkdir -pv "${MOZJS_BUILDDIR}"
}

src_configure() {
	# cp github.com/mozilla/cbindgen/releases/download/0.29.0/cbindgen-ubuntu22.04 /opt/rust-1.92.0/bin/opt/rust-bin-1.92.0/bin/cbindgen
	PATH=${RUSTROOT}/bin:${LLVMROOT}/bin:$PATH

	# Show flags set at the beginning
	einfo "Current BINDGEN_CFLAGS:\t${BINDGEN_CFLAGS:-no value set}"
	einfo "Current CFLAGS:    ${CFLAGS}"
	einfo "Current CXXFLAGS:  ${CXXFLAGS}"
	einfo "Current LDFLAGS:   ${LDFLAGS}"
	einfo "Current RUSTFLAGS: ${RUSTFLAGS}"

	local have_switched_compiler=
	if use clang; then # && ! tc-is-clang
		# Force clang
		einfo "Enforcing the use of clang due to USE=clang ..."

		local version_clang=$(clang --version 2>/dev/null | grep -F -- 'clang version' | awk '{ print $3 }')
		[[ -n ${version_clang} ]] && version_clang=$(ver_cut 1 "${version_clang}")
		[[ -z ${version_clang} ]] && die "Failed to read clang version!"

		if tc-is-gcc; then
			have_switched_compiler=yes
		fi

		AR=llvm-ar
		CC=${CHOST}-clang-${version_clang}
		CXX=${CHOST}-clang++-${version_clang}
		NM=llvm-nm
		RANLIB=llvm-ranlib
		READELF=llvm-readelf
		OBJDUMP=llvm-objdump

	elif ! use clang && ! tc-is-gcc ; then
		# Force gcc
		have_switched_compiler=yes
		einfo "Enforcing the use of gcc due to USE=-clang ..."
		AR=gcc-ar
		CC=${CHOST}-gcc
		CXX=${CHOST}-g++
		NM=gcc-nm
		RANLIB=gcc-ranlib
		READELF=readelf
		OBJDUMP=objdump
	fi

	if [[ -n "${have_switched_compiler}" ]] ; then
		# Because we switched active compiler we have to ensure
		# that no unsupported flags are set
		strip-unsupported-flags
	fi

	# Ensure we use correct toolchain
	export HOST_CC="$(tc-getBUILD_CC)"
	export HOST_CXX="$(tc-getBUILD_CXX)"
	export AS="$(tc-getCC) -c"

	tc-export CC CXX LD AR AS NM OBJDUMP RANLIB READELF PKG_CONFIG

	# Pass the correct toolchain paths through cbindgen
	if tc-is-cross-compiler ; then
		export BINDGEN_CFLAGS="${SYSROOT:+--sysroot=${ESYSROOT}} --target=${CHOST} ${BINDGEN_CFLAGS-}"
	fi

	# ../python/mach/mach/mixin/process.py fails to detect SHELL
	export SHELL="${EPREFIX}/bin/bash"

	# Set state path
	export MOZBUILD_STATE_PATH="${MOZJS_BUILDDIR}"

	# Set MOZCONFIG
	export MOZCONFIG="${S}/.mozconfig"

	# Initialize MOZCONFIG
	truncate -s 0 $MOZCONFIG
	mozconfig_add_options_ac '' --enable-project=js

	local -a myeconfargs=(
		--host="${CBUILD:-${CHOST}}"
		--target="${CHOST}"

		--disable-ctype
		--disable-jemalloc
		--disable-optimize
		--disable-smoosh
		--disable-strip

		--enable-readline
		--enable-release
		--enable-shared-js

		--libdir="${EPREFIX}/usr/$(get_libdir)"
		--prefix="${EPREFIX}/usr"

		--with-intl-api
		$(use-with system-icu)
		$(use-with system-nspr)
		--with-system-zlib
		--with-toolchain-prefix="${CHOST}-"

		$(use_enable debug)
		$(use_enable jit)
		$(use_enable test tests)
		$(use_enable debug real-time-tracing)
	)

	# We always end up disabling this at some point due to newer rust versions. bgo#933372
	myeconfargs+=( --disable-rust-simd )

	# Tell build system that we want to use LTO
	if use lto ; then
		if use clang ; then
			myeconfargs+=( --enable-linker=lld )
			myeconfargs+=( --enable-lto=cross )
		else
			myeconfargs+=( --enable-linker=bfd )
			myeconfargs+=( --enable-lto=full )
		fi
	fi

	# LTO flag was handled via configure
	filter-lto

	# Make cargo respect MAKEOPTS
	export MOZ_MAKE_FLAGS="$(makeopts_jobs)"

	# Use system's Python environment
	export MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE="none"
	export PIP_NETWORK_INSTALL_RESTRICTED_VIRTUALENVS=mach

	# Disable notification when build system has finished
	export MOZ_NOSPAM=1

	mozconfig_add_options_ac 'Gentoo default' "XARGS=${EPREFIX}/usr/bin/xargs"
	mozconfig_add_options_mk 'Gentoo default' "MOZ_OBJDIR=${MOZJS_BUILDDIR}"

	# Show flags we will use
	einfo "Build BINDGEN_CFLAGS:\t${BINDGEN_CFLAGS:-no value set}"
	einfo "Build CFLAGS:    ${CFLAGS}"
	einfo "Build CXXFLAGS:  ${CXXFLAGS}"
	einfo "Build LDFLAGS:   ${LDFLAGS}"
	einfo "Build RUSTFLAGS: ${RUSTFLAGS}"

	mozconfig_add_options_ac  'Gentoo default' \
		${myeconfargs[@]}

	./mach configure || die
}

src_compile() {
	cd "${MOZJS_BUILDDIR}" || die
	default
}

src_test() {
	return;
}

src_install() {
	cd "${MOZJS_BUILDDIR}" || die
	default

	# fix soname links
	pushd "${ED}"/usr/$(get_libdir) &>/dev/null || die
	mv lib${MY_PN}-${MY_MAJOR}.so lib${MY_PN}-${MY_MAJOR}.so.0.0.0 || die
	ln -s lib${MY_PN}-${MY_MAJOR}.so.0.0.0 lib${MY_PN}-${MY_MAJOR}.so.0 || die
	ln -s lib${MY_PN}-${MY_MAJOR}.so.0 lib${MY_PN}-${MY_MAJOR}.so || die
	popd &>/dev/null || die

	# remove unneeded files
	rm \
		"${ED}"/usr/bin/js${MY_MAJOR}-config \
		"${ED}"/usr/$(get_libdir)/libjs_static.ajs \
		|| die

	# fix permissions
	chmod -x \
		"${ED}"/usr/$(get_libdir)/pkgconfig/*.pc \
		"${ED}"/usr/include/mozjs-${MY_MAJOR}/js-config.h \
		|| die
}

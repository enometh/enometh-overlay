# Copyright 2022-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Dec 11 09:05:58 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 20190426 9999 wine-4.6-251-g8582615894
# ;madhu 201211 4.22-r2 -> wine-6.0-rc1-56-gbe459282420
# ;madhu 201213 wine-6.0-rc1-56-gbe459282420 (reverts 2 patches
# "winex11: Set res_class to the program name for Crostini." and
# "libwine: Ignore 0 weights as described in Unicode collation algorithm"
#
# ;madhu 210912 6.17 - abort
# ;madhu 211016 6.19 + patch to fix 16bit - wine-6.19-176-gaaaaeba9442
# ;madhu 211114 6.21 wine-6.21-216-g2c085c63d1c
# ;madhu 220817 7.15 wine-7.15-74-g4608e1e1257 (master)
# ;madhu 230622 8.10 wine-8.10-179-g81859c9af70 (unrolled patches)
# ;madhu 250806 10.12 -- inline wine.eclass (llvm-core BDEPENDS cannot be overridden by the ebuild)

EAPI=8
USE_GIT=true

inherit optfeature # wine
inherit autotools flag-o-matic multilib prefix toolchain-funcs wrapper
#WINE_USEDEP=( abi_x86_{32,64} )
readonly WINE_USEDEP="abi_x86_32(-)?,abi_x86_64(-)?"

IUSE="
	+abi_x86_32 +abi_x86_64 crossdev-mingw custom-cflags
	+mingw +strip wow64
"
REQUIRED_USE="
	|| ( abi_x86_32 abi_x86_64 arm64 )
	crossdev-mingw? ( mingw )
	wow64? ( !arm64? ( abi_x86_64 !abi_x86_32 ) )
"

RDEPEND="arm64? ( wow64? ( app-emulation/fex-xtajit ) )"
# 		llvm-core/lld:*
BDEPEND="
	|| (
		sys-devel/binutils:*
		dev-antichrist/llvmorg
	)
	dev-lang/perl
	mingw? (
		!crossdev-mingw? (
			wow64? ( dev-util/mingw64-toolchain[abi_x86_32] )
			!wow64? ( dev-util/mingw64-toolchain[${WINE_USEDEP}] )
		)
	)
	!mingw? (
	dev-antichrist/llvmorg
	)
"
#		llvm-core/clang:*
#		llvm-core/lld:*
#		strip? ( llvm-core/llvm:* )

IDEPEND=">=app-eselect/eselect-wine-2"

RDEPEND=" ${RDEPEND}
arm64? ( wow64? ( app-emulation/fex-xtajit ) )"
BDEPEND=" ${BDEPEND}
	|| (
		sys-devel/binutils:*
		dev-antichrist/llvmorg
	)
	dev-lang/perl
	mingw? (
		!crossdev-mingw? (
			wow64? ( dev-util/mingw64-toolchain[abi_x86_32] )
			!wow64? ( dev-util/mingw64-toolchain[${WINE_USEDEP}] )
		)
	)
	!mingw? (
		dev-antichrist/llvmorg
	)
"

WINE_GECKO=2.47.4
WINE_MONO=10.1.0

# downgrade
WINE_GECKO=2.47.3
WINE_MONO=7.3.0

MY_COMMIT=c767b6a641534a3c6c5ab2dab80290016421fcc5

if [[ ${PV} == 9999  || ${USE_GIT} ]]; then
	inherit git-r3
#	EGIT_REPO_URI="https://source.winehq.org/git/wine.git" (note: egit override changes)
	EGIT_REPO_URI="https://gitlab.winehq.org/wine/wine.git"
	EGIT_BRANCH=tip
	EGIT_CLONE_TYPE=shallow
	if [ -n ${MY_COMMIT} ]; then  EGIT_COMMIT="$MY_COMMIT" ; fi
	if ${USE_GIT}; then
			KEYWORDS="-* ~amd64 ~x86"
	fi
else
	(( $(ver_cut 2) )) && WINE_SDIR=$(ver_cut 1).x || WINE_SDIR=$(ver_cut 1).0
	SRC_URI="https://dl.winehq.org/wine/source/${WINE_SDIR}/wine-${PV}.tar.xz"
	S=${WORKDIR}/wine-${PV}
	KEYWORDS="-* ~amd64 ~arm64 ~x86"
fi

DESCRIPTION="Free implementation of Windows(tm) on Unix, without external patchsets"
HOMEPAGE="
	https://www.winehq.org/
	https://gitlab.winehq.org/wine/wine/
"

LICENSE="
	LGPL-2.1+
	BSD BSD-2 IJG MIT OPENLDAP ZLIB gsm libpng2 libtiff
	|| ( WTFPL-2 public-domain )
"
SLOT="${PV}"

IUSE=" ${IUSE}
	+X +alsa bluetooth capi cups +dbus dos llvm-libunwind ffmpeg
	+fontconfig +gecko gphoto2 +gstreamer kerberos +mono netapi
	nls odbc opencl +opengl pcap perl pulseaudio samba scanner
	+sdl selinux smartcard +ssl +truetype udev +unwind usb v4l
	+vulkan wayland +xcomposite xinerama
"

REQUIRED_USE=" ${REQUIRED_USE}
	X? ( truetype )
	bluetooth? ( dbus )
	opengl? ( || ( X wayland ) )
"

# tests are non-trivial to run, can hang easily, don't play well with
# sandbox, and several need real opengl/vulkan or network access
RESTRICT="test"

# `grep WINE_CHECK_SONAME configure.ac` + if not directly linked
# 	opengl? ( media-libs/libglvnd[X?,${WINE_USEDEP}] )
WINE_DLOPEN_DEPEND="
	X? (
		x11-libs/libXcursor[${WINE_USEDEP}]
		x11-libs/libXfixes[${WINE_USEDEP}]
		x11-libs/libXi[${WINE_USEDEP}]
		x11-libs/libXrandr[${WINE_USEDEP}]
		x11-libs/libXrender[${WINE_USEDEP}]
		x11-libs/libXxf86vm[${WINE_USEDEP}]
		xcomposite? ( x11-libs/libXcomposite[${WINE_USEDEP}] )
		xinerama? ( x11-libs/libXinerama[${WINE_USEDEP}] )
	)
	cups? ( net-print/cups[${WINE_USEDEP}] )
	dbus? ( sys-apps/dbus[${WINE_USEDEP}] )
	fontconfig? ( media-libs/fontconfig[${WINE_USEDEP}] )
	kerberos? ( virtual/krb5[${WINE_USEDEP}] )
	netapi? ( net-fs/samba[${WINE_USEDEP}] )
	odbc? ( dev-db/unixODBC[${WINE_USEDEP}] )
	sdl? ( media-libs/libsdl2[haptic,joystick,${WINE_USEDEP}] )
	ssl? ( net-libs/gnutls:=[${WINE_USEDEP}] )
	truetype? ( media-libs/freetype[${WINE_USEDEP}] )
	v4l? ( media-libs/libv4l[${WINE_USEDEP}] )
	vulkan? ( media-libs/vulkan-loader[X?,wayland?,${WINE_USEDEP}] )
"

# 		llvm-libunwind? ( llvm-runtimes/libunwind[${WINE_USEDEP}] )
WINE_COMMON_DEPEND="
	${WINE_DLOPEN_DEPEND}
	X? (
		x11-libs/libX11[${WINE_USEDEP}]
		x11-libs/libXext[${WINE_USEDEP}]
	)
	alsa? ( media-libs/alsa-lib[${WINE_USEDEP}] )
	capi? ( net-libs/libcapi:=[${WINE_USEDEP}] )
	ffmpeg? ( media-video/ffmpeg:=[${WINE_USEDEP}] )
	gphoto2? ( media-libs/libgphoto2:=[${WINE_USEDEP}] )
	gstreamer? (
		dev-libs/glib:2[${WINE_USEDEP}]
		media-libs/gst-plugins-base:1.0[${WINE_USEDEP}]
		media-libs/gstreamer:1.0[${WINE_USEDEP}]
	)
	opencl? ( virtual/opencl[${WINE_USEDEP}] )
	pcap? ( net-libs/libpcap[${WINE_USEDEP}] )
	pulseaudio? ( media-libs/libpulse[${WINE_USEDEP}] )
	scanner? ( media-gfx/sane-backends[${WINE_USEDEP}] )
	smartcard? ( sys-apps/pcsc-lite[${WINE_USEDEP}] )
	udev? ( virtual/libudev:=[${WINE_USEDEP}] )
	unwind? (
		llvm-libunwind? ( dev-antichrist/llvmorg )
		!llvm-libunwind? ( sys-libs/libunwind:=[${WINE_USEDEP}] )
	)
	usb? ( dev-libs/libusb:1[${WINE_USEDEP}] )
	wayland? (
		dev-libs/wayland[${WINE_USEDEP}]
		x11-libs/libxkbcommon[${WINE_USEDEP}]
	)
"
RDEPEND="
	${WINE_COMMON_DEPEND}
	app-emulation/wine-desktop-common
	dos? (
		|| (
			games-emulation/dosbox
			games-emulation/dosbox-staging
		)
	)
	gecko? (
		app-emulation/wine-gecko:${WINE_GECKO}[${WINE_USEDEP}]
		wow64? ( app-emulation/wine-gecko[abi_x86_32] )
	)
	gstreamer? ( media-plugins/gst-plugins-meta:1.0[${WINE_USEDEP}] )
	mono? ( app-emulation/wine-mono:${WINE_MONO} )
	perl? (
		dev-lang/perl
		dev-perl/XML-LibXML
	)
	samba? ( net-fs/samba[winbind] )
	selinux? ( sec-policy/selinux-wine )
"
DEPEND="
	${WINE_COMMON_DEPEND}
	sys-kernel/linux-headers
	X? ( x11-base/xorg-proto )
	bluetooth? ( net-wireless/bluez )
"
BDEPEND="
	sys-devel/bison
	sys-devel/flex
	virtual/pkgconfig
	nls? ( sys-devel/gettext )
	wayland? ( dev-util/wayland-scanner )
"

QA_CONFIG_IMPL_DECL_SKIP=(
	__clear_cache # unused on amd64+x86 (bug #900338)
	res_getservers # false positive
)
QA_TEXTRELS="usr/lib/*/wine/i386-unix/*.so" # uses -fno-PIC -Wl,-z,notext

PATCHES=(
	"${FILESDIR}"/${PN}-7.0-noexecstack.patch
	"${FILESDIR}"/${PN}-7.20-unwind.patch
	$FILESDIR/wine-10.12-revert-gecko-2.47.3-mono-7.3.0.patch
	$FILESDIR/wine-8.10-Revert-winex11-Set-res_class-to-the-program-name-for.patch
	"${FILESDIR}"/${PN}-8.13-rpath.patch
)

WINE_SKIP_INSTALL=()

# wine_pkg_pretend()
pkg_pretend()  {
	[[ ${MERGE_TYPE} == binary ]] && return

	if use crossdev-mingw && [[ ! -v MINGW_BYPASS ]]; then
		local arches=(
			$(usev abi_x86_64 x86_64)
			$(usev abi_x86_32 i686)
			$(usev wow64 i686)
			$(usev arm64 aarch64)
		)

		local mingw
		for mingw in "${arches[@]/%/-w64-mingw32}"; do
			if ! type -P ${mingw}-gcc >/dev/null; then
				eerror "With USE=crossdev-mingw, you must prepare the MinGW toolchain"
				eerror "yourself by installing sys-devel/crossdev then running:"
				eerror
				eerror "    crossdev --target ${mingw}"
				eerror
				eerror "For more information, please see: https://wiki.gentoo.org/wiki/Mingw"
				die "USE=crossdev-mingw is enabled, but ${mingw}-gcc was not found"
			fi
		done
	fi
}

# wine_src_prepare()
src_prepare() {
	default

	if [[ ${WINE_GECKO} && ${WINE_MONO} ]]; then
		# sanity check, bumping these has a history of oversights
		local geckomono=$(sed -En '/^#define (GECKO|MONO)_VER/{s/[^0-9.]//gp}' \
			dlls/appwiz.cpl/addons.c || die)

		if [[ ${WINE_GECKO}$'\n'${WINE_MONO} != "${geckomono}" ]]; then
			local gmfatal=
			has live ${PROPERTIES} && gmfatal=nonfatal
			${gmfatal} die -n "gecko/mono mismatch in ebuild, has: " ${geckomono} " (please file a bug)"
		fi
	fi

	if tc-is-clang && use mingw; then
		# -mabi=ms was ignored by <clang:16 then turned error in :17
		# if used without --target *-windows, then gets used in install
		# phase despite USE=mingw, drop as a quick fix for now
		sed -i '/MSVCRTFLAGS=/s/-mabi=ms//' configure.ac || die
	fi

	# ensure .desktop calls this variant + slot
	sed -i "/^Exec=/s/wine /${P} /" loader/wine.desktop || die

	# needed to find wine-mono on prefix
	hprefixify -w /get_mono_path/ dlls/mscoree/metahost.c

	# always update for patches (including user's wrt #432348)
	eautoreconf
	tools/make_requests || die # perl
}

_wine_flags() {
	local -n wcc=wcc_${2} wccflags=wcc_${2}_testflags

	case ${1} in
		c)
			# many hardening options are unlikely to work right
			filter-flags '-fstack-protector*' #870136
			filter-flags '-mfunction-return=thunk*' #878849

			# bashrc-mv users often do CFLAGS="${LDFLAGS}" and then
			# compile-only tests miss stripping unsupported linker flags
			filter-flags '-Wl,*'

			# -mavx with mingw-gcc has a history of problems and still see
			# users have issues despite Wine's -mpreferred-stack-boundary=2
			use mingw && append-cflags -mno-avx

			# same as strip-unsupported-flags but echos only for CC
			CC="${wcc} ${wccflags}" test-flags-CC ${CFLAGS}
		;;
		ld)
			# let compiler figure out the right linker for cross
			filter-flags '-fuse-ld=*'

			CC="${wcc} ${wccflags}" test-flags-CCLD ${LDFLAGS}
		;;
	esac
}

wine_src_configure() {
	WINE_PREFIX=/usr/lib/${P}
	WINE_DATADIR=/usr/share/${P}
	WINE_INCLUDEDIR=/usr/include/${P}

	local conf=(
		--prefix="${EPREFIX}"${WINE_PREFIX}
		--datadir="${EPREFIX}"${WINE_DATADIR}
		--includedir="${EPREFIX}"${WINE_INCLUDEDIR}
		--libdir="${EPREFIX}"${WINE_PREFIX}
		--mandir="${EPREFIX}"${WINE_DATADIR}/man
	)

	# strip-flags due to being generally fragile
	use custom-cflags || strip-flags

	# longstanding failing to build with lto, filter unconditionally
	filter-lto

	# may segfault at runtime if used (bug #931329)
	filter-flags -Wl,--gc-sections

	# avoid gcc-15's c23 default with older wine (bug #943849)
	ver_test -lt 10 && append-cflags -std=gnu17

	# Wine uses many linker tricks that are unlikely to work
	# with anything but bfd or lld (bug #867097)
	if ! tc-ld-is-bfd && ! tc-ld-is-lld; then
		has_version -b sys-devel/binutils &&
			append-ldflags -fuse-ld=bfd ||
			append-ldflags -fuse-ld=lld
		strip-unsupported-flags
	fi

	# wcc_* variables are used by _wine_flags(), see that
	# function if need to adjust *FLAGS only for cross
	local wcc_{amd64,x86,arm64}{,_testflags}
	# TODO?: llvm-mingw support if ever packaged and wanted
	if use mingw; then
		conf+=( --with-mingw )

		use !crossdev-mingw &&
			! has_version -b 'dev-util/mingw64-toolchain[bin-symlinks]' &&
			PATH=${BROOT}/usr/lib/mingw64-toolchain/bin:${PATH}

		wcc_amd64=${CROSSCC:-${CROSSCC_amd64:-x86_64-w64-mingw32-gcc}}
		wcc_x86=${CROSSCC:-${CROSSCC_x86:-i686-w64-mingw32-gcc}}
		# no mingw64-toolchain ~arm64, but "may" be usable with crossdev
		# (aarch64- rather than arm64- given it is what Wine searches for)
		wcc_arm64=${CROSSCC:-${CROSSCC_arm64:-aarch64-w64-mingw32-gcc}}
	else
		conf+=( --with-mingw=clang )

		# not building for ${CHOST} so $(tc-getCC) is not quite right, but
		# *should* support -target *-windows regardless (testflags is only
		# used by _wine_flags(), wine handles -target by itself)
		tc-is-clang && local clang=$(tc-getCC) || local clang=clang
		wcc_amd64=${CROSSCC:-${CROSSCC_amd64:-${clang}}}
		wcc_amd64_testflags="-target x86_64-windows"
		wcc_x86=${CROSSCC:-${CROSSCC_x86:-${clang}}}
		wcc_x86_testflags="-target i386-windows"
		wcc_arm64=${CROSSCC:-${CROSSCC_arm64:-${clang}}}
		wcc_arm64_testflags="-target aarch64-windows"

		# do not copy from regular LDFLAGS given odds are they all are
		# incompatible, and difficult to test linking without llvm-mingw
		: "${CROSSLDFLAGS:= }"
	fi

	conf+=(
		ac_cv_prog_x86_64_CC="${wcc_amd64}"
		ac_cv_prog_i386_CC="${wcc_x86}"
		ac_cv_prog_aarch64_CC="${wcc_arm64}"
	)

	if ver_test -ge 10; then
		# TODO: merge with the av_cv array above when <wine-10 is gone
		conf+=(
			# if set, use CROSS*FLAGS as-is without filtering
			x86_64_CFLAGS="${CROSSCFLAGS_amd64:-${CROSSCFLAGS:-$(_wine_flags c amd64)}}"
			x86_64_LDFLAGS="${CROSSLDFLAGS_amd64:-${CROSSLDFLAGS:-$(_wine_flags ld amd64)}}"
			i386_CFLAGS="${CROSSCFLAGS_x86:-${CROSSCFLAGS:-$(_wine_flags c x86)}}"
			i386_LDFLAGS="${CROSSLDFLAGS_x86:-${CROSSLDFLAGS:-$(_wine_flags ld x86)}}"
			aarch64_CFLAGS="${CROSSCFLAGS_arm64:-${CROSSCFLAGS:-$(_wine_flags c arm64)}}"
			aarch64_LDFLAGS="${CROSSLDFLAGS_arm64:-${CROSSLDFLAGS:-$(_wine_flags ld arm64)}}"
		)
	elif use abi_x86_64; then
		conf+=(
			# per-arch flags are only respected with >=wine-10,
			# do a one-arch best effort fallback
			CROSSCFLAGS="${CROSSCFLAGS_amd64:-${CROSSCFLAGS:-$(_wine_flags c amd64)}}"
			CROSSLDFLAGS="${CROSSLDFLAGS_amd64:-${CROSSLDFLAGS:-$(_wine_flags ld amd64)}}"
		)
	elif use abi_x86_32; then
		conf+=(
			CROSSCFLAGS="${CROSSCFLAGS_x86:-${CROSSCFLAGS:-$(_wine_flags c x86)}}"
			CROSSLDFLAGS="${CROSSLDFLAGS_x86:-${CROSSLDFLAGS:-$(_wine_flags ld x86)}}"
		)
	fi

	if use abi_x86_64 && use abi_x86_32 && use !wow64; then
		# multilib dual build method for "old" wow64 (must do 64 first)
		local bits
		for bits in 64 32; do
		(
			einfo "Configuring for ${bits}bits in ${WORKDIR}/build${bits} ..."

			mkdir ../build${bits} || die
			cd ../build${bits} || die

			if (( bits == 64 )); then
				conf+=( --enable-win64 )
			else
				conf+=(
					--with-wine64=../build64
					TARGETFLAGS=-m32 # for widl
				)

				# optional, but prefer over Wine's auto-detect (+#472038)
				multilib_toolchain_setup x86
			fi

			ECONF_SOURCE=${S} econf "${conf[@]}" "${wineconfargs[@]}"
		)
		done
	else
		# new --enable-archs method, or 32bit-only
		local archs=(
			$(usev abi_x86_64 x86_64)
			$(usev wow64 i386) # 32-on-64bit "new" wow64
			$(usev arm64 aarch64)
		)
		conf+=( ${archs:+--enable-archs="${archs[*]}"} )

		if use amd64 && use !abi_x86_64; then
			# same as above for 32bit-only on 64bit (allowed for wine)
			conf+=( TARGETFLAGS=-m32 )
			multilib_toolchain_setup x86
		fi

		econf "${conf[@]}" "${wineconfargs[@]}"
	fi
}

src_configure() {
	local wineconfargs=(
		$(use_enable gecko mshtml)
		$(use_enable mono mscoree)
		--disable-tests

		$(use_with X x)
		$(use_with alsa)
		$(use_with capi)
		$(use_with cups)
		$(use_with dbus)
		$(use_with ffmpeg)
		$(use_with fontconfig)
		$(use_with gphoto2 gphoto)
		$(use_with gstreamer)
		$(use_with kerberos gssapi)
		$(use_with kerberos krb5)
		$(use_with netapi)
		$(use_with nls gettext)
		$(use_with opencl)
		$(use_with opengl)
		--without-oss # media-sound/oss is not packaged (OSSv4)
		$(use_with pcap)
		$(use_with pulseaudio pulse)
		$(use_with scanner sane)
		$(use_with sdl)
		$(use_with smartcard pcsclite)
		$(use_with ssl gnutls)
		$(use_with truetype freetype)
		$(use_with udev)
		$(use_with unwind)
		$(use_with usb)
		$(use_with v4l v4l2)
		$(use_with vulkan)
		$(use_with wayland)
		$(use_with xcomposite)
		$(use_with xinerama)

		$(usev !bluetooth '
			ac_cv_header_bluetooth_bluetooth_h=no
			ac_cv_header_bluetooth_rfcomm_h=no
		')
		$(usev !odbc ac_cv_lib_soname_odbc=)
	)

	wine_src_configure
}

#wine_src_compile()
src_compile() {
	if use abi_x86_64 && use abi_x86_32 && use !wow64; then
		emake -C ../build64 # do first
		emake -C ../build32
	else
		emake
	fi
}

wine_src_install() {
	if use abi_x86_64 && use abi_x86_32 && use !wow64; then
		emake DESTDIR="${D}" -C ../build32 install
		emake DESTDIR="${D}" -C ../build64 install # do last
	else
		emake DESTDIR="${D}" install
	fi

	if use abi_x86_64 || use arm64; then
		if ver_test -ge 10.2; then
			# wine64 was removed, but keep a symlink for old scripts
			# TODO: can remove this -e guard eventually, only there to
			# avoid overwriting 9999's wine64 if go into <10.2 commits
			[[ ! -e ${ED}${WINE_PREFIX}/bin/wine64 ]] &&
				dosym wine ${WINE_PREFIX}/bin/wine64
		else
			# <wine-10.2 did not have a unified wine(1) and could miss
			# wine64 or wine depending on USE, ensure both are are there
			if [[ -e ${ED}${WINE_PREFIX}/bin/wine64 && ! -e ${ED}${WINE_PREFIX}/bin/wine ]]; then
				dosym wine64 ${WINE_PREFIX}/bin/wine
				dosym wine64-preloader ${WINE_PREFIX}/bin/wine-preloader
			elif [[ ! -e ${ED}${WINE_PREFIX}/bin/wine64 && -e ${ED}${WINE_PREFIX}/bin/wine ]]; then
				dosym wine ${WINE_PREFIX}/bin/wine64
				dosym wine-preloader ${WINE_PREFIX}/bin/wine64-preloader
			fi
		fi
	fi

	use arm64 && use wow64 &&
		dosym -r /usr/lib/fex-xtajit/libwow64fex.dll \
				${WINE_PREFIX}/wine/aarch64-windows/xtajit.dll

	# delete unwanted files if requested, not done directly in ebuilds
	# given must be done after install and before wrappers
	if (( ${#WINE_SKIP_INSTALL[@]} )); then
		rm -- "${WINE_SKIP_INSTALL[@]/#/${ED}}" || die
	fi

	# create variant wrappers for eselect-wine
	local bin
	for bin in "${ED}"${WINE_PREFIX}/bin/*; do
		make_wrapper "${bin##*/}-${P#wine-}" "${bin#"${ED}"}"
	done

	# don't let the package manager try to strip Windows files with
	# potentially the wrong strip executable and instead handle it here
	dostrip -x ${WINE_PREFIX}/wine/{x86_64,i386,aarch64}-windows

	if use strip; then
		ebegin "Stripping Windows binaries"
		if use mingw; then
			: "$(usex arm64 aarch64 $(usex abi_x86_64 x86_64 i686)-w64-mingw32-strip)"
			find "${ED}"${WINE_PREFIX}/wine/*-windows -regex '.*\.\(a\|dll\|exe\)' \
				-type f -exec ${_} --strip-unneeded {} +
		else
			# llvm-strip errors on .a, and CHOST binutils strip could mangle
			find "${ED}"${WINE_PREFIX}/wine/*-windows -regex '.*\.\(dll\|exe\)' \
				-type f -exec llvm-strip --strip-unneeded {} +
		fi
		eend ${?} || die
	fi
}

src_install() {
	use perl || local WINE_SKIP_INSTALL=(
		${WINE_DATADIR}/man/man1/wine{dump,maker}.1
		${WINE_PREFIX}/bin/{function_grep.pl,wine{dump,maker}}
	)

	wine_src_install

	dodoc ANNOUNCE* AUTHORS README* documentation/README*
}

wine_pkg_postinst() {
	# on amd64, users sometime disable the default 32bit support due to being
	# annoyed by the requirements without realizing that they need it
	if use amd64 && use !abi_x86_32 && use !wow64; then
		ewarn
		ewarn "32bit support is disabled. While 64bit applications themselves will"
		ewarn "work, be warned that it is not unusual that installers or other helpers"
		ewarn "will attempt to use 32bit and fail. If do not want full USE=abi_x86_32,"
		ewarn "note the experimental USE=wow64 can allow 32bit without full multilib."
	fi

	# difficult to tell what is needed from here, but try to warn anyway
	if use abi_x86_32 && { use opengl || use vulkan; }; then
		if has_version 'x11-drivers/nvidia-drivers'; then
			if has_version 'x11-drivers/nvidia-drivers[-abi_x86_32]'; then
				ewarn
				ewarn "x11-drivers/nvidia-drivers is installed but is built without"
				ewarn "USE=abi_x86_32 (ABI_X86=32), hardware acceleration with 32bit"
				ewarn "applications under ${PN} will likely not be usable."
				ewarn "Multi-card setups may need this on media-libs/mesa as well."
			fi
		elif has_version 'media-libs/mesa[-abi_x86_32]'; then
			ewarn
			ewarn "media-libs/mesa seems to be in use but is built without"
			ewarn "USE=abi_x86_32 (ABI_X86=32), hardware acceleration with 32bit"
			ewarn "applications under ${PN} will likely not be usable."
		fi
	fi

	if use arm64 && use wow64; then
		ewarn
		ewarn "You have enabled x86 emulation via FEX-Emu's xtajit implementation."
		ewarn "This currently *does not* include amd64/x86_64/x64 emulation. Only i386"
		ewarn "and ARM64 Windows applications are supported at this time. Please do not"
		ewarn "file bugs about amd64 applications."
	fi

	eselect wine update --if-unset || die
}

pkg_postinst() {
	wine_pkg_postinst

	optfeature "/dev/hidraw* access used for *some* controllers (e.g. DualShock4)" \
		games-util/game-device-udev-rules
}

#wine_pkg_postrm()
pkg_postrm() {
	if has_version -b app-eselect/eselect-wine; then
		eselect wine update --if-unset || die
	fi
}

# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Sep 15 20:02:34 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220915 1.2.7.1-r1 - USE=pipewire patches the plugin to quit if
# pipewire is found. USE=pulseaudio depends on pulseaudio instead of
# libpulse.
#
# ;madhu 241231 1.2.12
# ;madhu 250105 1.2.12 updated patch to fix EMFILE, debugging optional with env var ALSAPLUGIN_PWDEBUG=1
EAPI=8

inherit autotools flag-o-matic multilib-minimal

DESCRIPTION="ALSA extra plugins"
HOMEPAGE="https://alsa-project.org/wiki/Main_Page"
SRC_URI="https://www.alsa-project.org/files/pub/plugins/${P}.tar.bz2"

LICENSE="GPL-2 LGPL-2.1"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ppc ppc64 ~riscv sparc x86 ~amd64-linux"
IUSE="arcam_av debug ffmpeg jack libsamplerate +mix oss pipewire pulseaudio speex +usb_stream"

# ;madhu 220915 replace libpulse with pulseaudio
#	pulseaudio? ( media-libs/libpulse[${MULTILIB_USEDEP}] )

RDEPEND="
	>=media-libs/alsa-lib-${PV}:=[${MULTILIB_USEDEP}]
	ffmpeg? ( media-video/ffmpeg:0=[${MULTILIB_USEDEP}] )
	jack? ( virtual/jack[${MULTILIB_USEDEP}] )
	libsamplerate? ( >=media-libs/libsamplerate-0.1.8-r1:=[${MULTILIB_USEDEP}] )
	pulseaudio? ( >=media-sound/pulseaudio-2.1-r1[${MULTILIB_USEDEP}] )
	speex? (
		>=media-libs/speex-1.2.0:=[${MULTILIB_USEDEP}]
		media-libs/speexdsp[${MULTILIB_USEDEP}]
	)
	pipewire? ( >=media-video/pipewire-0.3.50[${MULTILIB_USEDEP}] )
"
DEPEND="${RDEPEND}"
BDEPEND="virtual/pkgconfig"

PATCHES=(
#	"${FILESDIR}"/${PN}-1.2.7.1-missing-include.patch
)

src_prepare() {
	default

	if use pipewire; then
		eapply ${FILESDIR}/${PN}-1.2.12-handle-pipewire-quit.patch
	fi

	# For some reasons the polyp/pulse plugin does fail with alsaplayer with a
	# failed assert. As the code works just fine with asserts disabled, for now
	# disable them waiting for a better solution.
	sed \
		-e '/AM_CFLAGS/s:-Wall:-DNDEBUG -Wall:' \
		-i pulse/Makefile.am || die

	eautoreconf
}

multilib_src_configure() {
	use debug || append-cppflags -DNDEBUG

	# ;madhu 220915; FIX qawarning by adding pipewire to alsa-plugins
	# autotools
	if use pipewire; then
	   append-cppflags -DHAVE_PIPEWIRE
	   append-ldflags $(pkg-config --libs libpipewire-0.3)
	   append-cflags $(pkg-config --cflags libpipewire-0.3)
	fi

	local myeconfargs=(
		# default does not contain $prefix: bug #673464
		--with-alsalconfdir="${EPREFIX}"/etc/alsa/conf.d

		--with-speex="$(usex speex lib no)"
		$(use_enable arcam_av arcamav)
		$(use_enable ffmpeg libav)
		$(use_enable jack)
		$(use_enable libsamplerate samplerate)
		$(use_enable mix)
		$(use_enable oss)
		$(use_enable pulseaudio)
		$(use_enable speex speexdsp)
		$(use_enable usb_stream usbstream)
	)
	ECONF_SOURCE="${S}" econf "${myeconfargs[@]}"
}

multilib_src_install() {
	# Needed to workaround parallel build failure
	# bug #835920
	dodir /usr/$(get_libdir)/alsa-lib

	default
}

multilib_src_install_all() {
	einstalldocs

	cd doc || die
	dodoc upmix.txt vdownmix.txt README-pcm-oss
	use jack && dodoc README-jack
	use libsamplerate && dodoc samplerate.txt
	use ffmpeg && dodoc lavrate.txt a52.txt

	if use pulseaudio; then
		dodoc README-pulse
		# install ALSA configuration files
		# making PA to be used by alsa clients
		insinto /usr/share/alsa
		doins "${FILESDIR}"/pulse-default.conf

		insinto /usr/share/alsa/alsa.conf.d
		doins "${FILESDIR}"/51-pulseaudio-probe.conf
		# bug #670960
		dosym ../../../usr/share/alsa/alsa.conf.d/51-pulseaudio-probe.conf \
			/etc/alsa/conf.d/51-pulseaudio-probe.conf
	fi

	find "${ED}" -type f \( -name '*.a' -o -name '*.la' \) -delete || die
}

pkg_postinst() {
	if use pulseaudio; then
		einfo "The PulseAudio device is now set as the default device if the"
		einfo "PulseAudio server is found to be running. Any custom"
		einfo "configuration in /etc/asound.conf or ~/.asoundrc for this"
		einfo "purpose should now be unnecessary."
	fi
}

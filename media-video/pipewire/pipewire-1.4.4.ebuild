# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Jun 11 03:56:00 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210330 pipewire-0.3.25::gentoo
# ;madhu 210611 pipewire-0.3.30
# ;madhu 210611 pipewire-0.3.30-r1 bring back debug sdl vulkan no patches
# ;madhu 210801 0.3.32
# ;madhu 211005 0.3.38
# ;madhu 211203 0.3.40-r1
# ;madhu 211231 0.3.42
# ;madhu 220106 0.3.42-r1
# ;madhu 220331 0.3.49
# ;madhu 220420 0.3.50-r1 - don't make alsa-plugins[pulseaudio] a hard dep.
# ;madhu 220915 0.3.57 - remove all gentoo sound-server nonsense. pipewire alsa and pulseaudio should coexist on the same system
# ;madhu 230224 0.3.66-r1 - keep IUSE=udev
# ;madhu 230602 0.3.61-r2
# ;madhu 230623 0.3.71-r3
# ;madhu 231120 0.3.85 -- don't bump webrtc, jack-sdk shouldn't conflict with jack-audio-connection-kit (just with jack2)
# ;madhu 240725 1.2.1 -- get rid of docs nonsense, bogus doxygen dep. USE=-gsettings with pulse.
# ;madhu 250604 1.4.4
EAPI=8

# 1. Please regularly check (even at the point of bumping) Fedora's packaging
# for needed backports at https://src.fedoraproject.org/rpms/pipewire/tree/rawhide.
#
# 2. Upstream also sometimes amend release notes for the previous release to mention
# needed patches, e.g. https://gitlab.freedesktop.org/pipewire/pipewire/-/tags/0.3.55#distros
#
# 3. Keep an eye on git master (for both PipeWire and WirePlumber) as things
# continue to move quickly. It's not uncommon for fixes to be made shortly
# after releases.

PYTHON_COMPAT=( python3_{10..13} )
inherit meson-multilib optfeature prefix python-any-r1 systemd tmpfiles udev

if [[ ${PV} == 9999 ]]; then
	EGIT_REPO_URI="https://gitlab.freedesktop.org/${PN}/${PN}.git"
	EGIT_BRANCH="${PV%.*}"
	inherit git-r3
else
	if [[ ${PV} == *_p* ]] ; then
		MY_COMMIT=""
		SRC_URI="https://gitlab.freedesktop.org/pipewire/pipewire/-/archive/${MY_COMMIT}/pipewire-${MY_COMMIT}.tar.bz2 -> ${P}.tar.bz2"
		S="${WORKDIR}"/${PN}-${MY_COMMIT}
	else
		SRC_URI="https://gitlab.freedesktop.org/${PN}/${PN}/-/archive/${PV}/${P}.tar.bz2"
	fi

	KEYWORDS="~amd64 ~arm ~arm64 ~loong ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86"
fi

DESCRIPTION="Multimedia processing graphs"
HOMEPAGE="https://pipewire.org/"

LICENSE="MIT LGPL-2.1+ GPL-2"
# ABI was broken in 0.3.42 for https://gitlab.freedesktop.org/pipewire/wireplumber/-/issues/49
SLOT="0/0.4"
# ;madhu 231120 axe sound-server
IUSE="bluetooth elogind dbus doc echo-cancel extra ffmpeg flatpak gstreamer gsettings ieee1394 jack-client jack-sdk liblc3 loudness lv2 man udev"
IUSE+=" modemmanager pipewire-alsa readline roc selinux ssl system-service systemd test v4l X zeroconf"

# Once replacing system JACK libraries is possible, it's likely that
# jack-client IUSE will need blocking to avoid users accidentally
# configuring their systems to send PW sink output to the emulated
# JACK's sink - doing so is likely to yield no audio, cause a CPU
# cycles consuming loop (and may even cause GUI crashes)!

#;madhu 230224 FIXME
# - TODO: There should be "sound-server? ( || ( alsa bluetooth ) )" here, but ALSA is always enabled
# - TODO: Pulseaudio alsa plugin performs runtime check that pulseaudio server connection will work
#   which provides adequate guarantee that alsa-lib will be able to provide audio services.
#   If that works, pulseaudio defaults are loaded into alsa-lib runtime replacing default PCM and CTL.
#   When pipewire-alsa will be able to perform similar check, pipewire-alsa can be enabled unconditionally.
# - ffmpeg is only used for pw-cat. We don't build the spa plugin which receives barely any activity.

#	!sound-server? ( !pipewire-alsa )

REQUIRED_USE="
	ffmpeg? ( extra )
	bluetooth? ( dbus )
	jack-sdk? ( !jack-client )
	modemmanager? ( bluetooth )
	system-service? ( systemd )
	jack-client? ( dbus )
"

RESTRICT="!test? ( test )"

BDEPEND="
	>=dev-build/meson-0.59
	virtual/pkgconfig
	dbus? ( dev-util/gdbus-codegen )
	doc? (
		${PYTHON_DEPS}
		app-text/doxygen
		media-gfx/graphviz
	)
	man? (
		${PYTHON_DEPS}
		app-text/doxygen
	)
"

# ;madhu 231120 axe sound-server
#	sound-server? ( !media-sound/pulseaudio-daemon )
#   jack-sdk? ( !media-sound/jack-audio-connection-kit)

# * While udev could technically be optional, it's needed for a number of options,
# and not really worth it, bug #877769.
#
# * Supports both legacy webrtc-audio-processing:2 and new webrtc-audio-processing:1.
# Automagic but :2 isn't yet packaged.
#
# * Older Doxygen (<1.9.8) may work but inferior output is created:
#   - https://gitlab.freedesktop.org/pipewire/pipewire/-/merge_requests/1778
#   - https://github.com/doxygen/doxygen/issues/9254
RDEPEND="
	acct-group/audio
	acct-group/pipewire
	media-libs/alsa-lib[${MULTILIB_USEDEP}]
	sys-libs/ncurses:=[unicode(+)]
	virtual/libintl[${MULTILIB_USEDEP}]
	udev? (	virtual/libudev[${MULTILIB_USEDEP}] )
	bluetooth? (
		dev-libs/glib
		media-libs/fdk-aac
		media-libs/libldac
		media-libs/libfreeaptx
		media-libs/opus
		media-libs/sbc
		>=net-wireless/bluez-4.101:=
		virtual/libusb:1
	)
	elogind? ( sys-auth/elogind )
	dbus? ( sys-apps/dbus[${MULTILIB_USEDEP}] )
	echo-cancel? ( media-libs/webrtc-audio-processing )
	extra? ( >=media-libs/libsndfile-1.0.20 )
	ffmpeg? ( media-video/ffmpeg:= )
	flatpak? ( dev-libs/glib )
	gstreamer? (
		>=dev-libs/glib-2.32.0:2
		>=media-libs/gstreamer-1.10.0:1.0
		media-libs/gst-plugins-base:1.0
	)
	gsettings? ( >=dev-libs/glib-2.26.0:2 )
	ieee1394? ( media-libs/libffado[${MULTILIB_USEDEP}] )
	jack-client? ( >=media-sound/jack2-1.9.10:2[dbus] )
	jack-sdk? (
		!media-sound/jack-audio-connection-kit
		!media-sound/jack2
	)
	liblc3? ( media-sound/liblc3 )
	loudness? ( media-libs/libebur128:=[${MULTILIB_USEDEP}] )
	lv2? ( media-libs/lilv )
	modemmanager? ( >=net-misc/modemmanager-1.10.0 )
	pipewire-alsa? ( >=media-libs/alsa-lib-1.2.10[${MULTILIB_USEDEP}] )
	roc? ( media-libs/roc-toolkit )
	readline? ( sys-libs/readline:= )
	selinux? ( sys-libs/libselinux )
	ssl? ( dev-libs/openssl:= )
	systemd? ( sys-apps/systemd )
	system-service? ( acct-user/pipewire )
	v4l? ( media-libs/libv4l )
	X? (
		media-libs/libcanberra
		x11-libs/libX11
		x11-libs/libXfixes
	)
	zeroconf? ( net-dns/avahi )
"

DEPEND="${RDEPEND}"

PDEPEND=">=media-video/wireplumber-0.5.2"

# Present RDEPEND that are currently always disabled due to the PW
# code using them being required to be disabled by Gentoo guidelines
# (i.e. developer binaries not meant for users) and unready code
#	media-libs/libsdl2
#	>=media-libs/vulkan-loader-1.1.69
#
# Ditto for DEPEND
#	>=dev-util/vulkan-headers-1.1.69

DOCS=( {README,INSTALL}.md NEWS )

if false; then
PATCHES=(
	"${FILESDIR}"/${PN}-0.3.25-enable-failed-mlock-warning.patch
)
fi

pkg_setup() {
	if use doc || use man ; then
		python-any-r1_pkg_setup
	fi
}

src_prepare() {
	default

	sed -i -e "s/^subdir('po')/#&/g" meson.build

#;madhu 230224 FIXME
	# Used for upstream backports
	if [[ ${PV} != *9999 && -d "${FILESDIR}"/${PV} ]] ; then
		eapply "${FILESDIR}"/${PV}
	fi
}

multilib_src_configure() {
	local logind=disabled
	if multilib_is_native_abi ; then
		if use systemd ; then
			logind=enabled
		elif use elogind ; then
			logind=enabled
		fi
	fi

	local emesonargs=(
		-Ddocdir="${EPREFIX}"/usr/share/doc/${PF}

		$(meson_feature dbus)
		$(meson_native_use_feature zeroconf avahi)
		$(meson_native_use_feature doc docs)
		$(meson_native_enabled examples) # TODO: Figure out if this is still important now that media-session gone
		$(meson_feature test tests)
		-Dinstalled_tests=disabled # Matches upstream; Gentoo never installs tests
		$(meson_feature ieee1394 libffado)
		$(meson_native_use_feature gstreamer)
		$(meson_native_use_feature gstreamer gstreamer-device-provider)
		$(meson_native_use_feature gsettings)
		$(meson_native_use_feature systemd)
#XXX
		-Dlogind=${logind}
		-Dlogind-provider=$(usex systemd 'libsystemd' 'libelogind')

		$(meson_native_use_feature system-service systemd-system-service)
		-Dsystemd-system-unit-dir="$(systemd_get_systemunitdir)"
		-Dsystemd-user-unit-dir="$(systemd_get_userunitdir)"

		$(meson_native_use_feature systemd systemd-user-service)
		$(meson_feature pipewire-alsa) # Allows integrating ALSA apps into PW graph
		$(meson_feature selinux)
		-Dspa-plugins=enabled
		-Dalsa=enabled # Allows using kernel ALSA for sound I/O (NOTE: media-session is gone so IUSE=alsa/spa_alsa/alsa-backend might be possible)
		-Dcompress-offload=disabled # TODO: tinycompress unpackaged
		-Daudiomixer=enabled # Matches upstream
		-Daudioconvert=enabled # Matches upstream
		$(meson_native_use_feature bluetooth bluez5)
		$(meson_native_use_feature bluetooth bluez5-backend-hsp-native)
		$(meson_native_use_feature bluetooth bluez5-backend-hfp-native)
		# https://gitlab.freedesktop.org/pipewire/pipewire/-/merge_requests/1379
		$(meson_native_use_feature modemmanager bluez5-backend-native-mm)
		$(meson_native_use_feature bluetooth bluez5-backend-ofono)
		$(meson_native_use_feature bluetooth bluez5-backend-hsphfpd)
		$(meson_native_use_feature bluetooth bluez5-codec-aac)
		$(meson_native_use_feature bluetooth bluez5-codec-aptx)
		$(meson_native_use_feature bluetooth bluez5-codec-ldac)
		$(meson_native_use_feature bluetooth bluez5-codec-g722)
		$(meson_native_use_feature bluetooth opus)
		$(meson_native_use_feature bluetooth bluez5-codec-opus)
		$(meson_native_use_feature bluetooth libusb) # At least for now only used by bluez5 native (quirk detection of adapters)
		$(meson_native_use_feature echo-cancel echo-cancel-webrtc) #807889
		-Dcontrol=enabled # Matches upstream
		-Daudiotestsrc=enabled # Matches upstream
		-Dffmpeg=disabled # Disabled by upstream and no major developments to spa/plugins/ffmpeg/ since May 2020
		$(meson_native_use_feature ffmpeg pw-cat-ffmpeg)
		$(meson_native_use_feature flatpak)
		-Dpipewire-jack=enabled # Allows integrating JACK apps into PW graph
		$(meson_native_use_feature jack-client jack) # Allows PW to act as a JACK client
		$(meson_use jack-sdk jack-devel)
		$(usex jack-sdk "-Dlibjack-path=${EPREFIX}/usr/$(get_libdir)" '')
		-Dsupport=enabled # Miscellaneous/common plugins, such as null sink
		-Devl=disabled # Matches upstream
		-Dtest=disabled # fakesink and fakesource plugins
		-Dbluez5-codec-lc3plus=disabled # unpackaged
		$(meson_native_use_feature liblc3 bluez5-codec-lc3)
		$(meson_feature loudness ebur128)
		$(meson_native_use_feature lv2)
		$(meson_native_use_feature v4l v4l2)
		-Dlibcamera=disabled # libcamera is not in Portage tree
		$(meson_native_use_feature roc)
		$(meson_native_use_feature readline)
		$(meson_native_use_feature ssl raop)
		-Dvideoconvert=enabled # Matches upstream
		-Dvideotestsrc=enabled # Matches upstream
		-Dvolume=enabled # Matches upstream
		-Dvulkan=disabled # Uses pre-compiled Vulkan compute shader to provide a CGI video source (dev thing; disabled by upstream)
		$(meson_native_use_feature extra pw-cat)
		$(meson_feature udev)
		-Dudevrulesdir="${EPREFIX}$(get_udevdir)/rules.d"
		-Dsdl2=disabled # Controls SDL2 dependent code (currently only examples when -Dinstalled_tests=enabled which we never install)
		-Dlibmysofa=disabled # libmysofa is unpackaged
		$(meson_native_use_feature extra sndfile) # Enables libsndfile dependent code (currently only pw-cat)
		-Dsession-managers="[]" # All available session managers are now their own projects, so there's nothing to build

#;madhu 230224 FIXME
		# We still have <5.16 kernels packaged in Gentoo and 6.1 (LTS) only
		# just became stable, with 5.15 being the previous LTS. Many people
		# are still on it.
		-Dpam-defaults-install=true

		# Just for bell sounds in X11 right now.
		$(meson_native_use_feature X x11)
		$(meson_native_use_feature X x11-xfixes)
		$(meson_native_use_feature X libcanberra)

		# TODO
		-Dsnap=disabled
	)

	# This installs the schema file for pulseaudio-daemon, iff we are replacing
	# the official sound-server
	if false; then # use !sound-server; then
		emesonargs+=( '-Dgsettings-pulse-schema=disabled' )
	else
		emesonargs+=(
			$(meson_native_use_feature gsettings gsettings-pulse-schema)
		)
	fi

	meson_src_configure
}

multilib_src_test() {
	meson_src_test --timeout-multiplier 10
}

multilib_src_install() {
	# Our custom DOCS do not exist in multilib source directory
	DOCS= meson_src_install
}

multilib_src_install_all() {
	einstalldocs

	if use pipewire-alsa; then
		dodir /etc/alsa/conf.d

		# Install pipewire conf loader hook
		insinto /usr/share/alsa/alsa.conf.d
		doins "${FILESDIR}"/99-pipewire-default-hook.conf
		eprefixify "${ED}"/usr/share/alsa/alsa.conf.d/99-pipewire-default-hook.conf

		# These will break if someone has /etc that is a symbolic link to a subfolder! See #724222
		# And the current dosym8 -r implementation is likely affected by the same issue, too.
		dosym ../../../usr/share/alsa/alsa.conf.d/50-pipewire.conf /etc/alsa/conf.d/50-pipewire.conf
		dosym ../../../usr/share/alsa/alsa.conf.d/99-pipewire-default-hook.conf /etc/alsa/conf.d/99-pipewire-default-hook.conf
	fi

	exeinto /etc/user/init.d
	newexe "${FILESDIR}"/pipewire.initd pipewire

	#FIXME ;madhu 230224 let wireplumber deal with this.
	# Enable required wireplumber alsa and bluez monitors
	if false; then
	if use sound-server; then
		newexe "${FILESDIR}"/pipewire-pulse.initd pipewire-pulse

		# Install sound-server enabler for wireplumber 0.5.0+ conf syntax
		insinto /etc/wireplumber/wireplumber.conf.d
		doins "${FILESDIR}"/gentoo-sound-server-enable-audio-bluetooth.conf
	fi
	fi

	if use system-service; then
		newtmpfiles - pipewire.conf <<-EOF || die
			d /run/pipewire 0755 pipewire pipewire - -
		EOF
	fi

	if ! use systemd; then
		insinto /etc/xdg/autostart
		newins "${FILESDIR}"/pipewire.desktop-r2 pipewire.desktop

		exeinto /usr/bin
		newexe "${FILESDIR}"/gentoo-pipewire-launcher.in-r3 gentoo-pipewire-launcher

		doman "${FILESDIR}"/gentoo-pipewire-launcher.1

		# Disable pipewire-pulse if sound-server is disabled.
		#;madhu 230224
		if false; then
		if ! use sound-server ; then
			sed -i -s '/pipewire -c pipewire-pulse.conf/s/^/#/' "${ED}"/usr/bin/gentoo-pipewire-launcher || die
		fi
		fi

		eprefixify "${ED}"/usr/bin/gentoo-pipewire-launcher
	fi
}

pkg_postrm() {
	use udev && udev_reload
}

pkg_preinst() {
	HAD_SOUND_SERVER=0
	HAD_SYSTEM_SERVICE=0

	if has_version "media-video/pipewire[sound-server(-)]" ; then
		HAD_SOUND_SERVER=1
	fi

	if has_version "media-video/pipewire[system-service(-)]" ; then
		HAD_SYSTEM_SERVICE=1
	fi
}

pkg_postinst() {
	use udev && udev_reload

	use system-service && tmpfiles_process pipewire.conf

	local ver
	for ver in ${REPLACING_VERSIONS} ; do
		if has_version kde-plasma/kwin[screencast] || has_version x11-wm/mutter[screencast] ; then
			# https://bugs.gentoo.org/908490
			# https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/3243
			ewarn "Please restart KWin/Mutter after upgrading PipeWire."
			ewarn "Screencasting may not work until you do."
		fi

		if ver_test ${ver} -le 0.3.66-r1 ; then
			elog ">=pipewire-0.3.66 uses the 'pipewire' group to manage permissions"
			elog "and limits needed to function smoothly:"
			elog
			elog "1. Please make sure your user is in the 'pipewire' group for"
			elog "the best experience with realtime scheduling (PAM limits behavior)!"
			elog "You can add your account with:"
			elog " usermod -aG pipewire <youruser>"
			elog
			elog "2. For the best experience with fast user switching, it is recommended"
			elog "that you remove your user from the 'audio' group unless you rely on the"
			elog "audio group for device access control or ACLs.:"
			elog " usermod -rG audio <youruser>"
			elog

			if ! use jack-sdk ; then
				elog
				elog "JACK emulation is incomplete and not all programs will work. PipeWire's"
				elog "alternative libraries have been installed to a non-default location."
				elog "To use them, put pw-jack <application> before every JACK application."
				elog "When using pw-jack, do not run jackd/jackdbus. However, a virtual/jack"
				elog "provider is still needed to compile the JACK applications themselves."
				elog
			fi

			if use systemd ; then
				ewarn
				ewarn "PipeWire daemon startup has been moved to a launcher script!"
				ewarn "Make sure that ${EROOT}/etc/pipewire/pipewire.conf either does not exist or no"
				ewarn "longer is set to start a session manager or PulseAudio compatibility daemon (all"
				ewarn "lines similar to '{ path = /usr/bin/pipewire*' should be commented out)"
				ewarn
				ewarn "Those manually starting /usr/bin/pipewire via .xinitrc or similar _must_ from"
				ewarn "now on start ${EROOT}/usr/bin/gentoo-pipewire-launcher instead! It is highly"
				ewarn "advised that a D-Bus user session is set up before starting the script."
				ewarn
			fi

			if false; then
			if  use sound-server &&	( has_version 'media-sound/pulseaudio[daemon]' || has_version 'media-sound/pulseaudio-daemon' ) ; then
				elog
				elog "This ebuild auto-enables PulseAudio replacement. Because of that, users"
				elog "are recommended to edit pulseaudio client configuration files:"
				elog "${EROOT}/etc/pulse/client.conf and ${EROOT}/etc/pulse/client.conf.d/enable-autospawn.conf"
				elog "if it exists, and disable autospawning of the original daemon by setting:"
				elog
				elog "  autospawn = no"
				elog
				elog "Please note that the semicolon (;) must _NOT_ be at the beginning of the line!"
				elog
				elog "Alternatively, if replacing PulseAudio daemon is not desired, edit"
				elog "${EROOT}/usr/bin/gentoo-pipewire-launcher by commenting out the relevant"
				elog "command:"
				elog
				elog "#${EROOT}/usr/bin/pipewire -c pipewire-pulse.conf &"
				elog
			fi
			fi

			if has_version 'net-misc/ofono' ; then
				ewarn "Native backend has become default. Please disable oFono via:"
				if systemd_is_booted ; then
					ewarn "systemctl disable ofono"
				else
					ewarn "rc-update delete ofono"
				fi
			fi
		fi
	done

	if false; then
	if [[ ${HAD_SOUND_SERVER} -eq 0 || -z ${REPLACING_VERSIONS} ]] ; then
		# TODO: We could drop most of this if we set up systemd presets?
		# They're worth looking into because right now, the out-of-the-box experience
		# is automatic on OpenRC, while it needs manual intervention on systemd.
		if use sound-server && use systemd ; then
			elog
			elog "When switching from PulseAudio, you may need to disable PulseAudio:"
			elog
			elog "  systemctl --user disable pulseaudio.service pulseaudio.socket"
			elog
			elog "To use PipeWire, the user units must be manually enabled"
			elog "by running this command as each user you use for desktop activities:"
			elog
			elog "  systemctl --user enable pipewire.socket pipewire-pulse.socket"
			elog
			elog "A reboot is recommended to avoid interferences from still running"
			elog "PulseAudio daemon."
			elog
			elog "Both new users and those upgrading need to enable WirePlumber"
			elog "for relevant users:"
			elog
			elog "  systemctl --user disable pipewire-media-session.service"
			elog "  systemctl --user --force enable wireplumber.service"
			elog
			elog "Root user may replace --user with --global to change system default"
			elog "configuration for all of the above commands."
			elog
		fi

		if ! use sound-server ; then
			ewarn
			ewarn "USE=sound-server is disabled! If you want PipeWire to provide"
			ewarn "your sound, please enable it. See the wiki at"
			ewarn "https://wiki.gentoo.org/wiki/PipeWire#Replacing_PulseAudio"
			ewarn "for more details."
			ewarn
		fi
	fi
	fi

	# ;madhu 231120 FIXME BOGUS CRAP
	if use system-service && [[ ${HAD_SYSTEM_SERVICE} -eq 0 || -z ${REPLACING_VERSIONS} ]] ; then
		ewarn
		ewarn "You have enabled the system-service USE flag, which installs"
		ewarn "the system-wide systemd units that enable PipeWire to run as a system"
		ewarn "service. This is more than likely NOT what you want. You are strongly"
		ewarn "advised not to enable this mode and instead stick with systemd user"
		ewarn "units. The default configuration files will likely not work out of the"
		ewarn "box, and you are on your own with configuration."
		ewarn
	fi

	elog "For latest tips and tricks, troubleshooting information, and documentation"
	elog "in general, please refer to https://wiki.gentoo.org/wiki/PipeWire"
	elog

	optfeature_header "The following can be installed for optional runtime features:"
	optfeature "restricted realtime capabilities via D-Bus" sys-auth/rtkit

	if # use sound-server &&
		! use pipewire-alsa; then
		optfeature "ALSA plugin to use PulseAudio interface for output" "media-plugins/alsa-plugins[pulseaudio]"
	fi
}

# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed May 01 09:12:54 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# bump from 3.30.1 to 3.32.0
# heavily locally patched - ignore upstream patch
# ;madhu 190709 3.33.0
# ;madhu 191016 3.34.1 - disable systemd wwan (modem manager glib)
# ;madhu 191130 3.34.1-r1 - debug in package.env
# ;madhu 200213 3.35.0 - provide a patchset
# ;madhu 200306 3.35.92 - turn on gvc in patchset
# (rm -rfv patches; mkdir patches; (cd patches && git format-patch master); tar cvfJ /gentoo/myrodistdir/gnome-settings-daemon-3.35.0-patchset.tar.xz patches/*)
# ;madhu 200703 3.36.1 - vanilla build -include everything except network manager - need to patch /usr/lib64/pkgconfig/libpulse-mainloop-glib.pc:
#;madhu 200306 need to fix libpulse-mainloop --libs to include -lpulse
# Libs: -L${libdir} -lpulse-mainloop-glib -lpulse -pthread
#
# ;madhu 200920 T15:14:51 3.38.0
# ;madhu 210116 40_alpha - no wacom, patch gsettings
# ;madhu 220130 42_alpha (not built)
# ;madhu 220324 41.2 42.1-3-gc2db0ec
# ;madhu 221024 43.0 43.0-11-g9ef1e28
# ;madhu 240215 46_beta 46.beta

EAPI=8
PYTHON_COMPAT=( python3_{10..12} )

inherit gnome.org gnome2-utils python-any-r1 meson udev virtualx xdg
inherit gnome-versioning

USE_GIT=true
MY_COMMIT=9ef1e2898d5e7b7308fa59112b762a93bf60302b

DESCRIPTION="Gnome Settings Daemon"
HOMEPAGE="https://gitlab.gnome.org/GNOME/gnome-settings-daemon"

if ${USE_GIT} ; then
	inherit git-r3
	EGIT_REPO_URI=https://gitlab.gnome.org/GNOME/gnome-settings-daemon
	EGIT_BRANCH=master
	EGIT_COMMIT=${MY_COMMIT}

#EGIT_OVERRIDE_REPO_GNOME_GNOME_SETTINGS_DAEMON=file:///build/git-mirror/gnome-settings-daemon.git
#EGIT_OVERRIDE_REPO_GNOME_LIBGNOME_VOLUME_CONTROL=file:///build/git-mirror/gvc.git

	EGIT_CLONE_TYPE=shallow
	SRC_URI=""
	EGIT_SUBMODULES=(subprojects/gvc)
	S=${WORKDIR}/${P}
else
	:
fi

LICENSE="GPL-2+ LGPL-2+"
SLOT="0"
IUSE="+colord +cups debug elogind input_devices_wacom  modemmanager networkmanager smartcard systemd test wayland geocrap weather pulse polkit upower"
RESTRICT="!test? ( test )"
REQUIRED_USE="^^ ( elogind systemd )"
KEYWORDS="~alpha amd64 ~arm ~arm64 ~ia64 ~loong ~ppc ~ppc64 ~riscv ~sparc ~x86 ~amd64-linux ~x86-linux"

COMMON_DEPEND="
 geocrap? (	>=sci-geosciences/geocode-glib-3.10:2 )
	>=dev-libs/glib-2.58:2
	>=gnome-base/gnome-desktop-3.37.1:3=
	>=gnome-base/gsettings-desktop-schemas-42
	>=x11-libs/gtk+-3.15.3:3[X,wayland?]
	>=dev-libs/libgweather-4.2.0:4=
	colord? ( >=x11-misc/colord-1.4.5:= )
	media-libs/libcanberra[gtk3]
geocrap? (	>=app-misc/geoclue-2.3.1:2.0 )
	>=x11-libs/libnotify-0.7.3
pulse? (	>=media-sound/pulseaudio-12.99.3[glib] )
polkit? (	>=sys-auth/polkit-0.114 )
upower? (	>=sys-power/upower-0.99.12:= )
	x11-libs/libX11
	>=x11-libs/libXfixes-6.0.0
	dev-libs/libgudev:=
	wayland? ( dev-libs/wayland )
	input_devices_wacom? (
		>=dev-libs/libwacom-0.7:=
		>=x11-libs/pango-1.20.0
		x11-libs/gdk-pixbuf:2
	)
	smartcard? ( app-crypt/gcr:4= )
	cups? ( >=net-print/cups-1.4[dbus] )
	modemmanager? (
		>=app-crypt/gcr-3.90.0:4=
		>=net-misc/modemmanager-1.0:=
	)
	networkmanager? ( >=net-misc/networkmanager-1.0 )
	media-libs/alsa-lib
	x11-libs/libXi
	x11-libs/libXext
	media-libs/fontconfig
	systemd? (
		>=sys-apps/systemd-243
	)
"
DEPEND="${COMMON_DEPEND}
	x11-base/xorg-proto
"
# logind needed for power and session management, bug #464944
RDEPEND="${COMMON_DEPEND}
	gnome-base/dconf
	elogind? ( sys-auth/elogind )
"
# rfkill requires linux/rfkill.h, thus linux-headers dep, not os-headers.
# If this package wants to work on other kernels, we need to make rfkill conditional instead
BDEPEND="
	sys-kernel/linux-headers
	dev-util/glib-utils
	dev-util/gdbus-codegen
	${PYTHON_DEPS}
	test? (
		dev-util/umockdev
		$(python_gen_any_dep '
			dev-python/pygobject:3[${PYTHON_USEDEP}]
			dev-python/python-dbusmock[${PYTHON_USEDEP}]
		')
		gnome-base/gnome-session
	)
	>=sys-devel/gettext-0.19.8
	virtual/pkgconfig
"

PATCHES=(
	"${FILESDIR}"/42.1-build-Make-wacom-optional-and-controllable-via-meson.patch
	"${FILESDIR}"/${PN}-3.38.1-build-Allow-NM-optional-on-Linux.patch
	"${FILESDIR}"/40-gsd-xsettings-toggle-fixed-entries.patch
)

python_check_deps() {
	if use test; then
		python_has_version "dev-python/pygobject:3[${PYTHON_USEDEP}]" &&
		python_has_version "dev-python/python-dbusmock[${PYTHON_USEDEP}]"
	fi
}

pkg_setup() {
	python-any-r1_pkg_setup
}

src_prepare() {
	default
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
}

src_configure() {
	local emesonargs=(
		-Dudev_dir="$(get_udevdir)"
		$(meson_use systemd)
		-Dalsa=true
		-Dgudev=true
		-Dgcr3=false
		$(meson_use colord)
		$(meson_use cups)
		$(meson_use networkmanager network_manager)
		-Drfkill=true
		$(meson_use smartcard)
		$(meson_use input_devices_wacom wacom)
		$(meson_use wayland)
		$(meson_use modemmanager wwan)
	)
	meson_src_configure
}

src_install() {
	meson_src_install
	# Don't auto-suspend by default on AC power
	insinto /usr/share/glib-2.0/schemas
	doins "${FILESDIR}"/org.gnome.settings-daemon.plugins.power.gschema.override
	# Avoid bogus
	local shitdir=${ED}/etc/xdg/autostart.disabled/
	mkdir -pv ${shitdir}
	#  Color Keyboard MediaKeys XSettings  Rfkill
	#for i in A11ySettings Datetime PrintNotifications Wacom Power Housekeeping  ScreensaverProxy Sharing Sound Smartcard; do
	#	mv -iv ${ED}/etc/xdg/autostart{,.disabled}/org.gnome.SettingsDaemon.$i.desktop
	#done
}

src_test() {
	virtx meson_src_test
}

pkg_postinst() {
	udev_reload
	xdg_pkg_postinst
	gnome2_schemas_update
}

pkg_postrm() {
	udev_reload
	xdg_pkg_postrm
	gnome2_schemas_update
}

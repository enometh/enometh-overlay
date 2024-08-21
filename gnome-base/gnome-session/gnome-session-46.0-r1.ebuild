# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Apr 30 00:36:39 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190430 - bump from 3.30.1-r1 to 3.32.0
# ;madhu 190430 NOTE: requires --session=SESSSION explicitly or it fails
# doesn't pick it up from envvars
#
# - add new files: xinitrc.d: 80-dbus to reuse a dbus session,
#  05-eat-shit-elogind-xdg-runtime to set XDG_RUNTIME_DIR specified by
#  the user. usr/share/gnome-sessions/sessions: rename the upstream
#  session "gnome.session" as "gnome-orig.session". provide
#  "gnome-session-base-shell.session" which just starts the gnome-shell
#  and link this to gnome.session.
#
# - flashback support. gnome-flashback.desktop restricts flashback to
# the current desktop. Install a new set of flash files to implement the
# flash session.
#
#/etc/conf.d/xdm
#
#    START_STOP_ARGS="-- -session /usr/lib64/X11/xdm/Xsession.flash"
#
#/etc/X11/Sessions/flash
#/usr/lib64/X11/xdm/Xsession.flash
#
#   both modified to invoke gnome-session --session=flash explicitly.
#   both log to /dev/xconsole (disabled)
#
#/usr/share/applications/gnome-flashback-debug.desktop
#/usr/share/applications/gnome-flashback-debug-init.desktop
#
#  remove restrictions on XDG_CURRENT_DESKTOP="GNOME-Flashback:GNOME"
#
#/usr/share/gnome-session/sessions
#
# ;madhu 190530 3.32.2-r1
# ;madhu 190912 3.34.0 - use patches from git
# ;madhu 191201 3.34.2 (debug build, tar up patches in myrosdistdir)
# ;madhu 200214 3.35.3
# (mkdir patches; (cd patches; git format-patch master); tar cvfJ /gentoo/myrodistdir/gnome-session-40.0-patchset.tar.xz patches/000*)
# ;madhu 200502 3.36.0
# ;madhu 210413 40.0
# ;madhu 220130 41.3
# ;madhu 240215 45.0-r1
# ;madhu 240821 46.0-r1

EAPI=8

inherit desktop  gnome.org gnome2-utils meson systemd xdg
inherit gnome-versioning

DESCRIPTION="Gnome session manager"
HOMEPAGE="https://gitlab.gnome.org/GNOME/gnome-session"

LICENSE="GPL-2+"
SLOT="0"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~ia64 ~loong ~ppc ~ppc64 ~riscv ~sparc ~x86 ~amd64-linux ~x86-linux"
IUSE="doc elogind systemd +flashback"
# 46.0 There is a no longer a null backend available othewise  ?? not ^^
REQUIRED_USE="?? ( elogind systemd )"

# 	media-libs/libglvnd[X]
COMMON_DEPEND="
	>=dev-libs/glib-2.46.0:2
	>=x11-libs/gtk+-3.22.0:3
	x11-libs/libICE
	x11-libs/libSM
	x11-libs/libX11
	>=gnome-base/gnome-desktop-3.34.2:3=
	>=dev-libs/json-glib-0.10
	media-libs/mesa
	media-libs/libepoxy
	x11-libs/libXcomposite
	systemd? ( >=sys-apps/systemd-242:0= )
	elogind? ( >=sys-auth/elogind-239.4 )
	flashback? (
				gnome-base/gnome-flashback
				x11-wm/metacity
				x11-terms/gnome-terminal
				gnome-base/gnome-panel
				)
"

# Pure-runtime deps from the session files should *NOT* be added here.
# >=gnome-settings-daemon-3.35.91 for UsbProtection required component.
# x11-misc/xdg-user-dirs{,-gtk} are needed to create the various XDG_*_DIRs, and
# create .config/user-dirs.dirs which is read by glib to get G_USER_DIRECTORY_*
# xdg-user-dirs-update is run during login (see 10-user-dirs-update-gnome below).
# sys-apps/dbus[X] is needed for session management.
# Our 90-xcursor-theme-gnome reads a setting from gsettings-desktop-schemas.
RDEPEND="${COMMON_DEPEND}
	>=gnome-base/gnome-settings-daemon-3.35.91
	>=gnome-base/gsettings-desktop-schemas-0.1.7
	sys-apps/dbus[elogind=,systemd=,X]

	x11-misc/xdg-user-dirs
	x11-misc/xdg-user-dirs-gtk
"
DEPEND="${COMMON_DEPEND}
	x11-libs/xtrans
"
BDEPEND="
	dev-libs/libxslt
	dev-util/gdbus-codegen
	>=sys-devel/gettext-0.19.8
	virtual/pkgconfig
	doc? (
		app-text/xmlto
		app-text/docbook-xml-dtd:4.1.2
	)
"

PATCHES=(
	"${FILESDIR}"/${PN}-46.0-meson-Support-elogind.patch
	${FILESDIR}/46-gnome-session-gsm-systemd.c-gsm_systemd_init-don-t-d.patch
	${FILESDIR}/46-gnome-session-gsm-systemd.c-allow-non-graphical-sess.patch
	${FILESDIR}/46-doc-dbus-meson.build-xsltproc-nonet.patch
)

src_prepare() {
	default
	xdg_environment_reset

	# Install USE=doc in ${PF} if enabled
	sed -i -e "s:meson\.project_name(), 'dbus':'${PF}', 'dbus':" doc/dbus/meson.build || die

	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
}

src_configure() {
	local emesonargs=(
		-Ddeprecation_flags=false
		-Dsession_selector=true # gnome-custom-session
		$(meson_use doc docbook)
		-Dman=true
		-Dsystemduserunitdir="$(systemd_get_userunitdir)"
	)
	meson_src_configure
}

src_install() {
	meson_src_install

#	dodir /etc/X11/Sessions
	exeinto /etc/X11/Sessions
	doexe "${FILESDIR}/Gnome"

	newmenu "${FILESDIR}/defaults.list-r6" gnome-mimeapps.list

#	dodir /etc/X11/xinit/xinitrc.d/
	exeinto /etc/X11/xinit/xinitrc.d/
	newexe "${FILESDIR}/15-xdg-data-gnome-r1" 15-xdg-data-gnome

	# This should be done here as discussed in bug #270852
	newexe "${FILESDIR}/10-user-dirs-update-gnome-r1" 10-user-dirs-update-gnome

	# Set XCURSOR_THEME from current dconf setting instead of installing
	# default cursor symlink globally and affecting other DEs (bug #543488)
	# https://bugzilla.gnome.org/show_bug.cgi?id=711703
	newexe "${FILESDIR}/90-xcursor-theme-gnome" 90-xcursor-theme-gnome
}

pkg_postinst() {
	xdg_pkg_postinst
	gnome2_schemas_update

	if ! has_version gnome-base/gdm && ! has_version x11-misc/sddm; then
		ewarn "If you use a custom .xinitrc for your X session,"
		ewarn "make sure that the commands in the xinitrc.d scripts are run."
	fi

	if ! use systemd && ! use elogind; then
		ewarn "You are building without systemd or elogind support."
		ewarn "gnome-session won't be able to correctly track and manage your session."
	fi
}

pkg_postrm() {
	xdg_pkg_postinst
	gnome2_schemas_update
}

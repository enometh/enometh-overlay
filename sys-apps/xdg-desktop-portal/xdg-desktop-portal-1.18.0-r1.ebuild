# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Jul 05 15:19:29 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 190705 Updated for 1.4.2 use-enable (nls geococation)
# ;madhu 190911 with pipewire
# ;madhu 191224 1.6.0 libportal via test
# ;madhu 200428 1.7.2
# ;madhu 200724 1.7.2-r1 pipewire 0.3.7
# ;madhu 220106 1.12.1
# ;madhu 230130 1.16.0
# ;madhu 231009 1.18.0-r1, fix patches, reinstate doc.

EAPI=8

PYTHON_COMPAT=( python3_{10..12} )

inherit meson python-any-r1 systemd

DESCRIPTION="Desktop integration portal"
HOMEPAGE="https://flatpak.org/ https://github.com/flatpak/xdg-desktop-portal"
SRC_URI="https://github.com/flatpak/${PN}/releases/download/${PV}/${P}.tar.xz"

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="~amd64 ~arm ~arm64 ~loong ~ppc ~ppc64 ~riscv ~x86"
IUSE="doc geolocation flatpak seccomp systemd test"
RESTRICT="!test? ( test )"
# Upstream expect flatpak to be used w/ seccomp and flatpak needs bwrap anyway
REQUIRED_USE="flatpak? ( seccomp )"

DEPEND="
	>=dev-libs/glib-2.66:2
	dev-libs/json-glib
	dev-python/docutils
	>=media-video/pipewire-0.3:=
	>=sys-fs/fuse-3.10.0:3[suid]
	x11-libs/gdk-pixbuf
	geolocation? ( >=app-misc/geoclue-2.5.3:2.0 )
	flatpak? ( sys-apps/flatpak )
	seccomp? ( sys-apps/bubblewrap )
	systemd? ( sys-apps/systemd )
"
RDEPEND="
	${DEPEND}
	sys-apps/dbus
"
BDEPEND="
	dev-util/gdbus-codegen
	sys-devel/gettext
	virtual/pkgconfig
	doc? (
		app-text/docbook-xml-dtd:4.3
		app-text/xmlto
	)
	test? (
		${PYTHON_DEPS}
		dev-libs/libportal
		$(python_gen_any_dep '
			dev-python/pytest[${PYTHON_USEDEP}]
			dev-python/pytest-xdist[${PYTHON_USEDEP}]
			dev-python/python-dbusmock[${PYTHON_USEDEP}]
		')
	)
"

PATCHES=(
	${FILESDIR}/${P}-meson-make-bwrap-optional.patch
	${FILESDIR}/${P}-meson-make-flatpak-optional.patch
)

src_prepare() {
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	default
}

pkg_setup() {
	use test && python-any-r1_pkg_setup
}

python_check_deps() {
	python_has_version "dev-python/pytest[${PYTHON_USEDEP}]" &&
	python_has_version "dev-python/pytest-xdist[${PYTHON_USEDEP}]" &&
	python_has_version "dev-python/python-dbusmock[${PYTHON_USEDEP}]"
}

src_configure() {

	local emesonargs=(
		-Ddbus-service-dir="${EPREFIX}/usr/share/dbus-1/services"
		-Dsystemd-user-unit-dir="$(systemd_get_userunitdir)"
		$(meson_feature flatpak)
		# Only used for tests
		$(meson_feature test libportal)
		$(meson_feature geolocation geoclue)
		$(meson_feature seccomp bwrap)
		$(meson_feature systemd)
		-Dflatpak-interfaces-dir="${EPREFIX}/usr/share/dbus-1/interfaces"
		$(meson_feature doc docbook-docs)
		# -Dxmlto-flags=
		-Ddatarootdir="${EPREFIX}/usr/share"
		-Dman-pages=enabled
		-Dinstalled-tests=false
		$(meson_feature test pytest)
	)

	meson_src_configure
}

pkg_postinst() {
	if ! has_version gui-libs/xdg-desktop-portal-lxqt && ! has_version gui-libs/xdg-desktop-portal-wlr && \
		! has_version kde-plasma/xdg-desktop-portal-kde && ! has_version sys-apps/xdg-desktop-portal-gnome && \
		! has_version sys-apps/xdg-desktop-portal-gtk; then
		elog "${PN} is not usable without any of the following XDP"
		elog "implementations installed:"
		elog "  gui-libs/xdg-desktop-portal-lxqt"
		elog "  gui-libs/xdg-desktop-portal-wlr"
		elog "  kde-plasma/xdg-desktop-portal-kde"
		elog "  sys-apps/xdg-desktop-portal-gnome"
		elog "  sys-apps/xdg-desktop-portal-gtk"
	fi
}

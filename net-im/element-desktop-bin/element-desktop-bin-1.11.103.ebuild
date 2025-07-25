# Copyright 2020-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Jun 11 20:37:24 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 220522 1.10.13
# ;madhu 240128 1.11.61
# ;madhu 250611 1.11.103

EAPI=8

inherit optfeature unpacker xdg

MY_PN="${PN/-bin}"
# MY_URI="https://packages.element.io/debian/pool/main/e/element-desktop"

DESCRIPTION="A glossy Matrix collaboration client for desktop (binary package)"
HOMEPAGE="https://element.io"
SRC_URI="https://packages.element.io/desktop/install/linux/glibc-x86-64/${MY_PN}-${PV}.tar.gz"

S="${WORKDIR}"

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="-* ~amd64 ~arm64"
RESTRICT="splitdebug"

RDEPEND="
	>=app-accessibility/at-spi2-core-2.46.0:2
	app-crypt/libsecret
	dev-db/sqlcipher
	dev-libs/expat
	dev-libs/glib:2
	dev-libs/nettle
	dev-libs/nspr
	dev-libs/nss
	media-libs/alsa-lib
	media-libs/mesa
	net-print/cups
	sys-apps/dbus
	virtual/udev
	x11-libs/cairo
	x11-libs/gdk-pixbuf:2
	x11-libs/gtk+:3
	x11-libs/libX11
	x11-libs/libXcomposite
	x11-libs/libXdamage
	x11-libs/libXext
	x11-libs/libXfixes
	x11-libs/libXrandr
	x11-libs/libdrm
	x11-libs/libxcb
	x11-libs/libxkbcommon
	x11-libs/pango"

QA_PREBUILT="opt/Element/chrome-sandbox
	opt/Element/chrome_crashpad_handler
	opt/Element/element-desktop
	opt/Element/libEGL.so
	opt/Element/libGLESv2.so
	opt/Element/libffmpeg.so
	opt/Element/libvk_swiftshader.so
	opt/Element/libvulkan.so.1
	opt/Element/resources/app.asar.unpacked/node_modules/matrix-seshat/index.node
	opt/Element/resources/app.asar.unpacked/node_modules/keytar-forked/build/Release/keytar.node"

src_prepare() {
	default
}

src_install() {

	d=${ED}/opt/${MY_PN}-${MY_PV} # if SLOT=${PV}
	d=${ED}/opt/Element
	mkdir -pv ${ED}/opt
	mv ${MY_PN}-${PV} ${d}
	rm -fv ${ED}/opt/Element/{LICENSE.electron.txt,LICENSES.chromium.html}
	(cd ${d}/locales && ls | egrep -v 'en-US.pak' |xargs rm -fv)
	# fperns and dosym know about ED?
	fperms u+s /opt/Element/chrome-sandbox
	dosym ../../opt/Element/${MY_PN} /usr/bin/${MY_PN}
}

pkg_postinst() {
	xdg_pkg_postinst
	optfeature "emojis" media-fonts/noto-emoji
}

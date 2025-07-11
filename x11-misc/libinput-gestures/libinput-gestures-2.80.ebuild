# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Jul 10 20:22:22 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 210727 2.56
# ;madhu 240602 2.76
# ;madhu 250710 2.80

EAPI=8

PYTHON_COMPAT=( python3_{10..14} python3_{13..14}t )

inherit python-single-r1 xdg

DESCRIPTION="Actions gestures on your touchpad using libinput"
HOMEPAGE="https://github.com/bulletmark/libinput-gestures"
SRC_URI="https://github.com/bulletmark/${PN}/archive/${PV}.tar.gz -> ${P}.tar.gz"

LICENSE="GPL-3+"
SLOT="0"
KEYWORDS="~amd64 ~x86"
#IUSE="experimental"
REQUIRED_USE="${PYTHON_REQUIRED_USE}"
RESTRICT="test"

RDEPEND="${PYTHON_DEPS}
	dev-libs/libinput
	x11-misc/wmctrl
	x11-misc/xdotool"
DEPEND="dev-libs/libinput
	dev-util/desktop-file-utils"

PATCHES=(
	"${FILESDIR}/${PN}"-2.76-zombie.patch
	"${FILESDIR}/${PN}"-2.80-dont-exit-on-dbus.patch
)

src_prepare() {
	default

	# Fix docdir installation path
	sed -i "/^DOCDIR/s@\$NAME@${PF}@" libinput-gestures-setup \
		|| die "sed failed for libinput-gestures-setup"
}

src_compile() { :; }

src_test() {
	emake test
}

src_install() {
	default
	# Actually respect the python target setting
	python_doscript "${PN}"
}

pkg_postinst() {
	xdg_icon_cache_update

	elog "You must be in the input group to read the touchpad device."

	if ! has_version x11-libs/gtk+:3 ; then
		elog "${PN}-setup script supports GNOME via x11-libs/gtk+:3."
	fi
	if ! has_version kde-plasma/kde-cli-tools:6 ; then
		elog "${PN}-setup script supports Plasma via kde-plasma/kde-cli-tools:6."
	fi
}

# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#
#   Time-stamp: <>
#   Touched: Sun Dec 26 15:09:09 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# https://data.gpo.zugaina.org/waffle-builds/sys-apps/touchegg/touchegg-2.0.11.ebuild
#
# ;madhu 211226 2.0.12 2.0.12-7-g5166e31
# ;madhu 240619 2.0.17 2.0.17-3-g686bff3

EAPI=7

USE_GIT=true

inherit cmake

DESCRIPTION="Linux multi-touch gesture recognizer"
HOMEPAGE="https://github.com/JoseExposito/touchegg"

if [[ "${PV}" == 9999 ]] || ${USE_GIT} ; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/JoseExposito/touchegg.git"
	EGIT_CLONE_TYPE="shallow"

else
	SRC_URI="https://github.com/JoseExposito/${PN}/archive/refs/tags/${PV}.tar.gz -> ${P}.tar.gz"
fi

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="+gtk systemd"
RESTRICT="primaryuri"

RDEPEND="
	dev-libs/libinput
	dev-libs/pugixml
	x11-libs/cairo
	x11-libs/libX11
	x11-libs/libXtst
	x11-libs/libXrandr
	x11-libs/libXi
	dev-libs/glib:2
	gtk? ( x11-libs/gtk+:3 )
	virtual/libudev
	systemd? ( sys-apps/systemd )
"

DEPEND="${RDEPEND}"

DOCS=( "README.md" )

src_configure() {
	local mycmakeargs=(
		-DAUTO_COLORS="$(usex gtk)"
		-DUSE_SYSTEMD="$(usex systemd)"
	)

	cmake_src_configure
}

src_install() {
	default

	cmake_src_install

	newinitd "${FILESDIR}"/touchegg.initd touchegg
}

pkg_postinst() {
	elog "On update run: 'systemctl daemon-reload && systemctl restart touchegg'"
	elog "See https://github.com/JoseExposito/touchegg#configuration for more information."
	elog "For manual config, copy '/usr/share/${PN}/${PN}.conf' to '~/.config/${PN}'"
}
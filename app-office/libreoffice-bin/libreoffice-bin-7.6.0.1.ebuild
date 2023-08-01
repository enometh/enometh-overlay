# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2023-08-01 04:38:58 MDT>
#   Touched: Wed Jul 12 01:09:37 PM IST 2023 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230712 - 7.6.0.1 - amd64 only. install upstream build under
# ED with dpkg, use fakeroot to accomodate prefix. doesn't install
# stuff under /usr/share.

EAPI=8

inherit  xdg-utils

DESCRIPTION="Upstream libreoffice builds build"
HOMEPAGE="https://www.libreoffice.org"

# handle PV stuff later
SRC_URI="https://download.documentfoundation.org/libreoffice/testing/7.6.0/deb/x86_64/LibreOffice_7.6.0.1_Linux_x86-64_deb.tar.gz"

IUSE="gnome java kde"
LICENSE="LGPL-3"
SLOT="0"
KEYWORDS="-* amd64"

# fakeroot for prefix
BDEPEND="
	app-arch/dpkg
	sys-apps/fakeroot
"
S="${WORKDIR}"
QA_PREBUILT="/opt/*"


src_configure() { :; }

src_compile() { :; }

src_install() {
    if [ $(id -u) -eq 0 ]; then
	FAKEROOT=
    else
	FAKEROOT=fakeroot
    fi
    cd ${WORKDIR}/LibreOffice_7.6.0.1_Linux_x86-64_deb/DEBS || die
    $FAKEROOT dpkg --root=${ED} -i $(sed -e 's|^\(.*\)$|\1_7.6.0.1-1_amd64.deb|g'  ${FILESDIR}/loffice-7.6.0.1.list) || die
    rm -rf ${ED}/var || die
}

pkg_postinst() {
	xdg_icon_cache_update
	xdg_desktop_database_update
	xdg_mimeinfo_database_update
}

pkg_postrm() {
	xdg_icon_cache_update
	xdg_desktop_database_update
	xdg_mimeinfo_database_update
}

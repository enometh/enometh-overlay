#
#   Time-stamp: <2022-06-17 07:00:24 IST>
#   Touched: Mon Feb 04 06:30:45 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 200303 xdg-utils, JSC=1
# ;madhu 220617 MY_COMMIT=bab516c810a02fc06109bdd6bde3cd8bae1227f1

EAPI=7

inherit git-r3 toolchain-funcs xdg-utils

DESCRIPTION="Webkit based browser from jun7 "
HOMEPAGE="https://github.com/jun7/wyeb"

SRC_URI=""
#EGIT_REPO_URI="https://github.com/jun7/wyeb"
EGIT_REPO_URI="https://github.com/enometh/wyeb"
EGIT_BRANCH="madhu-libsoup-3"
S="${EGIT_CHECKOUT_DIR}"
EGIT_CLONE_TYPE="shallow"
EGIT_SUBMODULES=()

LICENSE="GPL3"
SLOT="0"
KEYWORDS="amd64 x86"
IUSE="soup3 gtk4"

DEPEND="net-libs/webkit-gtk[soup3?,gtk4?]"

src_prepare() {
	default
	if use amd64; then
		sed -i -e 's|/lib/wyebrowser|/lib64/wyebrowser|g' makefile
	fi
	local WKMAJ=4 WKMIN=0
	if use gtk4; then WKMAJ=5; fi;
	if use soup3; then WKMIN=1; fi;
	sed -i -e "s|webkit2gtk-4.1|webkit2gtk-$WKMAJ.$WKMIN|g" makefile
}

pkg_postinst() {
	 xdg_desktop_database_update
}

pkg_postrm() {
	 xdg_desktop_database_update
}

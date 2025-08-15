# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Aug 18 20:24:12 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220818 - xterm-372: PRIVATE repo: USE_GIT, ship basic PATCHES
# ;madhu 250815 - xterm-401, patches now document they are from suse. added BackArrow2* scripts, terminal.png (from suse), 16color.txt (from rawhide)

EAPI=8
USE_GIT=true

inherit desktop flag-o-matic toolchain-funcs xdg

DESCRIPTION="Terminal Emulator for X Windows"
HOMEPAGE="https://invisible-island.net/xterm/"
if ${USE_GIT}; then
	inherit git-r3
	SRC_URI=""
	EGIT_REPO_URI="file:///build/git-mirror/xterm.git"
	EGIT_BRANCH=master
else
SRC_URI="ftp://ftp.invisible-island.net/${PN}/${P}.tgz"
fi
LICENSE="MIT"
SLOT="0"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~loong ~m68k ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~x64-solaris"
IUSE="+openpty sixel toolbar truetype unicode Xaw3d xinerama"

BDEPEND="virtual/pkgconfig
	x11-base/xorg-proto"
DEPEND="
	kernel_linux? ( sys-libs/libutempter )
	media-libs/fontconfig:1.0
	media-libs/freetype
	>=sys-libs/ncurses-5.7-r7:0=
	x11-apps/xmessage
	x11-libs/libICE
	x11-libs/libX11
	x11-libs/libXaw
	x11-libs/libXft
	x11-libs/libxkbfile
	x11-libs/libXmu
	x11-libs/libXrender
	x11-libs/libXt
	unicode? ( x11-apps/luit )
	Xaw3d? ( x11-libs/libXaw3d )
	xinerama? ( x11-libs/libXinerama )"
RDEPEND="${DEPEND}
	media-fonts/font-misc-misc
	x11-apps/rgb"

DOCS=( README{,.i18n} ctlseqs.txt )

PATCHES=(
${FILESDIR}/xterm-double_width_fonts.patch.patch
${FILESDIR}/xterm-desktop_file_icon.patch.patch
${FILESDIR}/xterm-forbid_window_and_font_ops.patch.patch
${FILESDIR}/xterm-allow_iso-utf_fonts_in_menu.patch.patch
${FILESDIR}/xterm-decomposed_bitmaps.patch.patch
${FILESDIR}/xterm-settings.patch.patch
)

src_configure() {
	DEFAULTS_DIR="${EPREFIX}"/usr/share/X11/app-defaults

	# bug #454736
	# Workaround for ncurses[tinfo] until upstream fixes their buildsystem using
	# something sane like pkg-config or ncurses5-config and stops guessing libs
	# Everything gets linked against ncurses anyways, so don't shout
	append-libs $($(tc-getPKG_CONFIG) --libs ncurses)

	local myeconfargs=(
		--disable-full-tgetent
		--disable-imake
		--disable-setgid
		--disable-setuid
		--enable-256-color
		--enable-broken-osc
		--enable-broken-st
		--enable-dabbrev
		--enable-exec-xterm
		--enable-i18n
		--enable-load-vt-fonts
		--enable-logging
		--enable-screen-dumps
		--enable-warnings
		--enable-wide-chars
		--libdir="${EPREFIX}"/etc
		--with-app-defaults="${DEFAULTS_DIR}"
		--with-icon-theme=hicolor
		--with-icondir="${EPREFIX}"/usr/share/icons
		--with-utempter
		--with-x
		$(use_enable openpty)
		$(use_enable sixel sixel-graphics)
		$(use_enable toolbar)
		$(use_enable truetype freetype)
		$(use_enable unicode luit)
		$(use_enable unicode mini-luit)
		$(use_with Xaw3d)
		$(use_with xinerama)
	)

	econf "${myeconfargs[@]}"
}

src_install() {
	default

	dobin ${FILESDIR}/Backarrow2{BackSpace,Delete}
	dodoc ctlseqs.txt ${FILESDIR}/16colors.txt
	docinto html
	dodoc xterm.log.html
	sed -i -e 's/_48x48//g' *.desktop || die
	domenu *.desktop
	# xterm.desktop icon is patched
	doicon ${FILESDIR}/terminal.png #suse

	# Fix permissions -- it grabs them from live system, and they can
	# be suid or sgid like they were in pre-unix98 pty or pre-utempter days,
	# respectively (#69510).
	# (info from Thomas Dickey) - Donnie Berkholz <spyderous@gentoo.org>
#	fperms 0755 /usr/bin/xterm

	# restore the navy blue
#	sed -i -e 's:blue2$:blue:' "${D}${DEFAULTS_DIR}"/XTerm-color || die
}

# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2023-01-03 20:09:00 IST>
#   Touched: Sun Nov 15 09:45:42 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 201115 1.20.8-r1 -> 1.20.9 libglvnd nonsense
# ;madhu 210124 1.20.10 sysconfdir=/etc not etc/X11. Gentoo is not
#                the vendor anymore because they force libglvnd
# ;madhu 210413 1.20.11 display-manager-init
# ;madhu 210727 1.20.12 - dont use x11-base/xwayland yet
# ;madhu 210823 1.20.13 USE=-wayland
# ;madhu 211227 20.1.2 only libglvnd, wayland dmx kdrive gonn
# ;madhu 220221 20.1.3
# ;madhu 230103 20.1.6
# ;madhu 240308 20.1.11 - punt on flex
# ;madhu 241214 20.1.14
# ;madhu 250815 20.1.18
# ;madhu 251214 20.1.21
EAPI=8

XORG_TARBALL_SUFFIX="xz"
XORG_EAUTORECONF="no"
inherit flag-o-matic xorg-3 meson
EGIT_REPO_URI="https://gitlab.freedesktop.org/xorg/xserver.git"

DESCRIPTION="X.Org X servers"
SLOT="0/${PV}"
if [[ ${PV} != 9999* ]]; then
	KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~loong ~m68k ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86 ~amd64-linux ~x86-linux"
fi

IUSE_SERVERS="xephyr xnest xorg xvfb"
IUSE="${IUSE_SERVERS} debug +elogind ipv6 minimal selinux suid systemd test +udev unwind xcsecurity libglvnd"
RESTRICT="!test? ( test )"

CDEPEND="
libglvnd? (
		media-libs/libglvnd[X]
		!app-eselect/eselect-opengl
		!!x11-drivers/nvidia-drivers[-libglvnd(-)]
	)
	!libglvnd? ( >=app-eselect/eselect-opengl-1.3.0	)
	dev-libs/libbsd
	dev-libs/openssl:0=
	>=x11-apps/iceauth-1.0.2
	>=x11-apps/xauth-1.0.3
	x11-apps/xkbcomp
	>=x11-libs/libdrm-2.4.89
	>=x11-libs/libpciaccess-0.12.901
	>=x11-libs/libXau-1.0.4
	>=x11-libs/libXdmcp-1.0.2
	>=x11-libs/libXfont2-2.0.1
	>=x11-libs/libxkbfile-1.0.4
	>=x11-libs/libxshmfence-1.1
	>=x11-libs/pixman-0.27.2
	>=x11-misc/xbitmaps-1.0.1
	>=x11-misc/xkeyboard-config-2.4.1-r3
	xorg? (
		>=x11-libs/libxcvt-0.1.0
	)
	xnest? (
		>=x11-libs/libXext-1.0.99.4
		>=x11-libs/libX11-1.1.5
	)
	xephyr? (
		x11-libs/libxcb
		x11-libs/xcb-util
		x11-libs/xcb-util-image
		x11-libs/xcb-util-keysyms
		x11-libs/xcb-util-renderutil
		x11-libs/xcb-util-wm
	)
	!minimal? (
		>=media-libs/mesa-18[X(+),egl(+),gbm(+)]
		>=media-libs/libepoxy-1.5.4[X,egl(+)]
	)
	udev? ( virtual/libudev:= )
	unwind? ( sys-libs/libunwind:= )
	selinux? (
		sys-process/audit
		sys-libs/libselinux:=
	)
	systemd? (
		sys-apps/dbus
		sys-apps/systemd
	)
	elogind? (
		sys-apps/dbus
		sys-auth/elogind[pam]
		sys-auth/pambase[elogind]
	)
	!!x11-drivers/nvidia-drivers[-libglvnd(+)]
"
DEPEND="${CDEPEND}
	>=x11-base/xorg-proto-2021.4.99.2
	>=x11-libs/xtrans-1.3.5
	media-fonts/font-util
	test? ( >=x11-libs/libxcvt-0.1.0 )
"
RDEPEND="${CDEPEND}
	!systemd? ( gui-libs/display-manager-init )
	selinux? ( sec-policy/selinux-xserver )
	xorg? ( >=x11-apps/xinit-1.3.3-r1 )
"
# 	app-alternatives/lex
BDEPEND="
	sys-devel/flex
"
PDEPEND="
	xorg? ( >=x11-base/xorg-drivers-$(ver_cut 1-2) )"

REQUIRED_USE="!minimal? (
		|| ( ${IUSE_SERVERS} )
	)
	elogind? ( udev )
	?? ( elogind systemd )"

UPSTREAMED_PATCHES=(
)

PATCHES=(
	"${UPSTREAMED_PATCHES[@]}"
	"${FILESDIR}"/${PN}-1.12-unloadsubmodule.patch
	# needed for new eselect-opengl, bug #541232
	"${FILESDIR}"/${PN}-1.18-support-multiple-Files-sections.patch
	# pending upstream backport, bug #885763
# ;madhu 240308
#	"${FILESDIR}"/${PN}-21.1.10-c99.patch
)

src_configure() {
	# bug #835653
	use x86 && replace-flags -Os -O2
	use x86 && replace-flags -Oz -O2

	# localstatedir is used for the log location; we need to override the default
	#	from ebuild.sh
	# sysconfdir is used for the xorg.conf location; same applies
	local emesonargs=(
		--localstatedir "${EPREFIX}/var"
		--sysconfdir "${EPREFIX}/etc/X11"
		-Dbuildtype=$(usex debug debug plain)
		-Db_ndebug=$(usex debug false true)
		$(meson_use !minimal dri1)
		$(meson_use !minimal dri2)
		$(meson_use !minimal dri3)
		$(meson_use !minimal glamor)
		$(meson_use !minimal glx)
		$(meson_use udev)
		$(meson_use udev udev_kms)
		$(meson_use unwind libunwind)
		$(meson_use xcsecurity)
		$(meson_use selinux xselinux)
		$(meson_use xephyr)
		$(meson_use xnest)
		$(meson_use xorg)
		$(meson_use xvfb)
		-Ddocs=false
		-Ddrm=true
		-Ddtrace=false
		-Dipv6=true
		-Dhal=false
		-Dlinux_acpi=false
		-Dlinux_apm=false
		-Dsecure-rpc=false
		-Dsha1=libcrypto
		-Dxkb_output_dir="${EPREFIX}/var/lib/xkb"
	)

	if [[ ${PV} == 9999 ]] ; then
		# Gone in 21.1.x, but not in master.
		emesonargs+=( -Dxwayland=false )
	fi

	if use systemd || use elogind; then
		emesonargs+=(
			-Dsystemd_logind=true
			$(meson_use suid suid_wrapper)
		)
	else
		emesonargs+=(
			-Dsystemd_logind=false
			-Dsuid_wrapper=false
		)
	fi

	meson_src_configure
}

src_install() {
	meson_src_install

	# The meson build system does not support install-setuid
	if ! use systemd && ! use elogind; then
		if use suid; then
			chmod u+s "${ED}"/usr/bin/Xorg
		fi
	fi

	if ! use xorg; then
		rm -f "${ED}"/usr/share/man/man1/Xserver.1x \
			"${ED}"/usr/$(get_libdir)/xserver/SecurityPolicy \
			"${ED}"/usr/$(get_libdir)/pkgconfig/xorg-server.pc \
			"${ED}"/usr/share/man/man1/Xserver.1x || die
	fi

	# install the @x11-module-rebuild set for Portage
	insinto /usr/share/portage/config/sets
	newins "${FILESDIR}"/xorg-sets.conf xorg.conf
}

pkg_postrm() {
	# Get rid of module dir to ensure opengl-update works properly
	if [[ -z ${REPLACED_BY_VERSION} && -e ${EROOT}/usr/$(get_libdir)/xorg/modules ]]; then
		rm -rf "${EROOT}"/usr/$(get_libdir)/xorg/modules
	fi
}

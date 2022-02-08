# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2021-03-29 12:36:42 IST>
#   Touched: Fri Apr 26 13:26:46 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# ;madhu 90427 -need a way to pass EXTRA_ECONF to meson
# system-uid-max system-gid-max

# ;madhu 190511 2.241.2-r1 remove po files
# ;madhu 190810 2.241.3-r1 - add debug-extra
# ;madhu 200131 2.241.4
# ;madhu 200203 2.243.9999
# ;madhu 200215 2.244.9999
# ;madhu 200228 2.244.9999-> 243.7 from git -branch madhu
# ;madhu 200313 1cd3fe, drop ${PN}-243.4-docs.patch
# ;madhu 200627 - fix zombie, update ebuild, efi
# ;madhu 200703 243.8
# ;madhu 200901 246.0
# ;madhu 201002 246.0 - last was a dud because  git3-src/shallow was missing
# ;madhu 210116 246.9
# ;madhu 210329 246.9.2 v246-pre-79-g7a54937
# ;madhu 210506 246.10 on the 246-stable branch
# ;madhu 211005 248.0 v248-pre-9-g2fd46629
# ;madhu 220208 249.0_pre v249-pre-14-g0f29ee86 +cgroup-hybrid


EAPI=7
USE_GIT=true

inherit linux-info meson pam udev xdg-utils

if ${USE_GIT}; then
	inherit git-r3
	EGIT_BRANCH="madhu" #make sure its not set in package.env
	EGIT_REPO_URI="https://github.com/elogind/elogind.git"
	EGIT_SUBMODULES=()
	SRC_URI=
else
	SRC_URI="https://github.com/${PN}/${PN}/archive/v${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~ia64 ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86"
fi

inherit linux-info meson pam udev xdg-utils

DESCRIPTION="The systemd project's logind, extracted to a standalone package"
HOMEPAGE="https://github.com/elogind/elogind"

LICENSE="CC0-1.0 LGPL-2.1+ public-domain"
SLOT="0"
KEYWORDS="~amd64 ~arm ~x86"
IUSE="+acl audit +cgroup-hybrid debug doc +pam +policykit selinux efi"

BDEPEND="
	app-text/docbook-xml-dtd:4.2
	app-text/docbook-xml-dtd:4.5
	app-text/docbook-xsl-stylesheets
	dev-util/gperf
	dev-util/intltool
	virtual/pkgconfig
"
DEPEND="
	audit? ( sys-process/audit )
	sys-apps/util-linux
	sys-libs/libcap
	virtual/libudev:=
	acl? ( sys-apps/acl )
	pam? ( sys-libs/pam )
	selinux? ( sys-libs/libselinux )
"
RDEPEND="${DEPEND}
	!sys-apps/systemd
"
PDEPEND="
	sys-apps/dbus
	policykit? ( sys-auth/polkit )
"

DOCS=( README.md )
#we don't apply the docs patch
#DOCS += ( src/libelogind/sd-gvabus/GVARIANT-SERIALIZATION )

PATCHES=(
)

pkg_setup() {
	local CONFIG_CHECK="~CGROUPS ~EPOLL ~INOTIFY_USER ~SIGNALFD ~TIMERFD"

	use kernel_linux && linux-info_pkg_setup
}

src_prepare() {
	default
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	xdg_environment_reset
}

src_configure() {
	# local rccgroupmode="$(grep rc_cgroup_mode ${EPREFIX}/etc/rc.conf | cut -d '"' -f 2)"
	if use cgroup-hybrid; then
		cgroupmode="hybrid"
	else
		cgroupmode="unified"
	fi
	local debugmode=""

	if use debug; then
		debugmode="-Ddebug-extra=['elogind']" #'hashmap'
	fi

	# Duplicating C[XX]FLAGS in LDFLAGS is deprecated and will become
	# a hard error in future meson versions:
	filter-ldflags $CFLAGS $CXXFLAGS

	local emesonargs=(
		-Ddocdir="${EPREFIX}/usr/share/doc/${PF}"
		-Dhtmldir="${EPREFIX}/usr/share/doc/${PF}/html"
#bogus
		-Defi=$(usex efi true false)
		$debugmode
		-Dpamlibdir=$(getpam_mod_dir)
		-Dudevrulesdir="${EPREFIX}$(get_udevdir)"/rules.d
		--libdir="${EPREFIX}"/usr/$(get_libdir)
		-Drootlibdir="${EPREFIX}"/$(get_libdir)
		-Drootlibexecdir="${EPREFIX}"/$(get_libdir)/elogind
		-Drootprefix="${EPREFIX}/"
		-Dbashcompletiondir="${EPREFIX}/usr/share/bash-completion/completions"
		-Dman=auto
		-Dsmack=true
		-Dcgroup-controller=openrc
		-Ddefault-hierarchy=${cgroupmode}
		-Ddefault-kill-user-processes=false
		-Dacl=$(usex acl true false)
		--buildtype $(usex debug debug release)
# html docs are a mess of broken symlinks
#		-Dhtml=$(usex doc auto false)
		-Daudit=$(usex audit true false)
		-Dpam=$(usex pam true false)
		-Dselinux=$(usex selinux true false)
		-Dutmp=$(usex elibc_musl false true)

		-Dzshcompletiondir="${EPREFIX}/usr/share/zsh/site-functions"
		-Dsystem-uid-max=499
		-Dsystem-gid-max=499
#		-Ddebug-extra="['elogind','hashmap']"
	)

	meson_src_configure
}

src_install() {

	meson_src_install

	newinitd "${FILESDIR}"/${PN}.init-r1 ${PN}

	sed -e "s|@libdir@|$(get_libdir)|" "${FILESDIR}"/${PN}.conf.in > ${PN}.conf || die
	newconfd ${PN}.conf ${PN}
}

pkg_postinst() {
	if ! use pam; then
		ewarn "${PN} will not be managing user logins/seats without USE=\"pam\"!"
		ewarn "In other words, it will be useless for most applications."
		ewarn
	fi
	if ! use policykit; then
		ewarn "loginctl will not be able to perform privileged operations without"
		ewarn "USE=\"policykit\"! That means e.g. no suspend or hibernate."
		ewarn
	fi
	if [[ "$(rc-config list boot | grep elogind)" != "" ]]; then
		elog "elogind is currently started from boot runlevel."
	elif [[ "$(rc-config list default | grep elogind)" != "" ]]; then
		ewarn "elogind is currently started from default runlevel."
		ewarn "Please remove elogind from the default runlevel and"
		ewarn "add it to the boot runlevel by:"
		ewarn "# rc-update del elogind default"
		ewarn "# rc-update add elogind boot"
	else
		elog "elogind is currently not started from any runlevel."
		elog "You may add it to the boot runlevel by:"
		elog "# rc-update add elogind boot"
		elog
		elog "Alternatively, you can leave elogind out of any"
		elog "runlevel. It will then be started automatically"
		if use pam; then
			elog "when the first service calls it via dbus, or"
			elog "the first user logs into the system."
		else
			elog "when the first service calls it via dbus."
		fi
	fi
}

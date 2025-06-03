# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2025-01-21 18:03:49 IST>
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
# ;madhu 221212 250.0_pre
# ;madhu 230115 252.0_pre
# ;madhu 240116 254.0_pre
# ;madhu 240507 255.0_pre loses merged-usr, git patches installation of emptydirs, udev_reload
# ;madhu 250121 255.5-r1 v255-pre-70-g1da78237, fix jinja2 in deps
# ;madhu 250603 255.5-r2 v255.17-66-g712c0ff6d
EAPI=8
USE_GIT=true
PYTHON_COMPAT=( python3_{9..13} )

if ${USE_GIT}; then
	inherit git-r3
	EGIT_BRANCH="madhu-v255-stable" #make sure its not set in package.env
	EGIT_REPO_URI="https://github.com/elogind/elogind.git"
	EGIT_SUBMODULES=()
	SRC_URI=
else
	SRC_URI="https://github.com/${PN}/${PN}/archive/v${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~mips ppc ppc64 ~riscv ~s390 sparc x86"
fi

inherit eapi9-ver linux-info meson pam python-any-r1 udev xdg-utils

DESCRIPTION="The systemd project's logind, extracted to a standalone package"
HOMEPAGE="https://github.com/elogind/elogind"

LICENSE="CC0-1.0 LGPL-2.1+ public-domain"
SLOT="0"
IUSE="+acl audit +cgroup-hybrid debug doc +pam +policykit selinux efi test"
KEYWORDS="~alpha amd64 arm arm64 ~hppa ~loong ~mips ppc ppc64 ~riscv ~s390 sparc x86"
RESTRICT="!test? ( test )"

BDEPEND="
	app-text/docbook-xml-dtd:4.2
	app-text/docbook-xml-dtd:4.5
	app-text/docbook-xsl-stylesheets
	dev-util/gperf
	virtual/pkgconfig
	$(python_gen_any_dep 'dev-python/jinja2[${PYTHON_USEDEP}]')
	$(python_gen_any_dep 'dev-python/lxml[${PYTHON_USEDEP}]')
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

# we don't apply the docs patch, GVARIANT_SERIALIZATION is installed by meson
#DOCS += ( src/libelogind/sd-gvabus/GVARIANT-SERIALIZATION )

PATCHES=(
)

python_check_deps() {
	python_has_version "dev-python/jinja2[${PYTHON_USEDEP}]" &&
	python_has_version "dev-python/lxml[${PYTHON_USEDEP}]"
}

pkg_setup() {
	local CONFIG_CHECK="~CGROUPS ~EPOLL ~INOTIFY_USER ~SIGNALFD ~TIMERFD"

	use kernel_linux && linux-info_pkg_setup
}

src_prepare() {
	default
	sed -i -e "s/^subdir('po')/#subdir('po')/g" meson.build
	xdg_environment_reset

	# don't cleanup /dev/shm/ on logout on logout - handled by git branch
}

src_configure() {
	# local rccgroupmode="$(grep rc_cgroup_mode ${EPREFIX}/etc/rc.conf | cut -d '"' -f 2)"
	if use cgroup-hybrid; then
		cgroupmode="hybrid"
	else
		cgroupmode="unified"
	fi

	python_setup

	local debugmode=""
	if use debug; then
		debugmode="-Ddebug-extra=['elogind']" #'hashmap'
#;madhu 240507
		EMESON_BUILDTYPE="debug"
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
		-Dudevrulesdir="${EPREFIX}$(get_udevdir)"/rules.d
		--libdir="${EPREFIX}"/usr/$(get_libdir)
		--localstatedir="${EPREFIX}"/var
#;madhu 250121 - gentoo says --libexecdir=/lib/elogind
#;madhu 240507 - merged-usr shennanigans
#		-Drootlibdir="${EPREFIX}"/$(get_libdir)
#		-Drootlibexecdir="${EPREFIX}"/$(get_libdir)/elogind
#		-Drootprefix="${EPREFIX}/"
		-Dbashcompletiondir="${EPREFIX}/usr/share/bash-completion/completions"
		-Dman=auto
		-Dsmack=true
		-Dcgroup-controller=openrc
		-Ddefault-hierarchy=${cgroupmode}
		-Ddefault-kill-user-processes=false
		-Dacl=$(usex acl enabled disabled)
		-Daudit=$(usex audit enabled disabled)
# html docs are a mess of broken symlinks
#		-Dhtml=$(usex doc auto false)
##;madhu 240507 - portage forces the use of the EMESON_BUILDTYPE envvar
# 		--buildtype $(usex debug debug release)
		$(meson_feature pam)
		-Dpamlibdir=$(getpam_mod_dir)
		-Dselinux=$(usex selinux enabled disabled)
		-Dtests=$(usex test true false)
		-Dutmp=$(usex elibc_musl false true)

#		-Dmode=release

		# Ensure consistency between merged-usr and split-usr (bug 945965)
		-Dhalt-path="${EPREFIX}/sbin/halt"
		-Dkexec-path="${EPREFIX}/usr/sbin/kexec"
		-Dnologin-path="${EPREFIX}/sbin/nologin"
		-Dpoweroff-path="${EPREFIX}/sbin/poweroff"
		-Dreboot-path="${EPREFIX}/sbin/reboot"

		-Dzshcompletiondir="${EPREFIX}/usr/share/zsh/site-functions"
		-Dsystem-uid-max=499
		-Dsystem-gid-max=499
#		-Ddebug-extra="['elogind','hashmap']"
	)

	meson_src_configure
}

src_install() {
	meson_src_install
	keepdir /var/lib/elogind

	newinitd "${FILESDIR}"/${PN}.init-r1 ${PN}

	#;madhu 250121
	sed -e "s|@libexecdir@|usr/libexec|" "${FILESDIR}"/${PN}.conf.in-r1 > ${PN}.conf || die
	newconfd ${PN}.conf ${PN}
}

pkg_postinst() {
	udev_reload
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

pkg_postinst() {
	udev_reload
}

pkg_postrm() {
	udev_reload
}
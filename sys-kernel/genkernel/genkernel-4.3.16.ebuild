# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu May 14 08:24:06 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200514 -9999 to hotfix zfs with existing deps
# ;madhu 200803 -9999 madhu-v4.1.0-beta1
# genkernel-9999        -> latest Git branch "master"
# genkernel-VERSION     -> normal genkernel release
# ;madhu 201001 v4.1.2-14-g2dd063c USE_GIT sync with 4.1.2
# ;madhu 210301 v4.1.2-r3 v4.1.2-44-g282b201
# ;madhu 210327 v4.2.1-6-g4c70529
# ;madhu 210728 v4.2.3-5-g057b941
# ;madhu 211005 v4.2.4-8-g839fb7f
# ;madhu 211215 v4.2.6-7-ge14fc65
# ;madhu 230803 v4.3.5-17-gf08c5d4 -- fix install phase to not touch ${S}, do not ship distfiles. symlink distfiles to distdir manually (not checked in). catch git describe failures in shallow repositories.
# ;madhu 240913 4.3.15-9-g70ba0d8

EAPI=8

PYTHON_COMPAT=( python3_{10..12} )
USE_GIT=true

inherit bash-completion-r1 python-single-r1

# Whenever you bump a GKPKG, check if you have to move
# or add new patches!
VERSION_BCACHE_TOOLS="1.0.8_p20141204" #"1.1_p20230217"
# boost-1.84.0 needs dev-build/b2 packaged
VERSION_BOOST="1.74.0" #"1.79.0"
VERSION_BTRFS_PROGS="5.9" #"6.7.1"
VERSION_BUSYBOX="1.36.1"
VERSION_COREUTILS="9.5" #"9.4"
VERSION_CRYPTSETUP="2.7.4" #"2.6.1"
VERSION_DMRAID="1.0.0.rc16-3"
VERSION_DROPBEAR="2020.81" #"2022.83"
VERSION_EUDEV="3.2.10"
VERSION_EXPAT="2.6.3" #"2.5.0"
VERSION_E2FSPROGS="1.47.0"
VERSION_FUSE="2.9.9"
# gnupg-2.x needs several new deps packaged
VERSION_GPG="1.4.23"
VERSION_HWIDS="20210613"
# open-iscsi-2.1.9 static build not working yet
VERSION_ISCSI="2.0.878" #"2.1.8"
# json-c-0.17 needs gkbuild ported to meson
VERSION_JSON_C="0.17"
VERSION_KMOD="31"
VERSION_LIBAIO="0.3.112" #"0.3.113"
VERSION_LIBGCRYPT="1.9.3" #"1.10.3"
VERSION_LIBGPGERROR="1.42" #"1.47"
VERSION_LIBXCRYPT="4.4.36"
VERSION_LVM="2.03.22"
VERSION_LZO="2.10"
VERSION_MDADM="4.3" #"4.2"
VERSION_POPT="1.19"
VERSION_STRACE="6.10" # "6.7"
VERSION_THIN_PROVISIONING_TOOLS="0.9.0"
# unionfs-fuse-3.4 needs fuse:3
VERSION_UNIONFS_FUSE="2.0"
VERSION_USERSPACE_RCU="0.10.2" #"0.14.0"
VERSION_UTIL_LINUX="2.38" #"2.39.3"
VERSION_XFSPROGS="5.10.0" #"6.4.0"
VERSION_XZ="5.6.1" #"5.4.2"
VERSION_ZLIB="1.2.11" #"1.3.1"
VERSION_ZSTD="1.5.6" #"1.5.5"
VERSION_KEYUTILS="1.6.3"

#	https://dev.gentoo.org/~blueness/eudev/eudev-${VERSION_EUDEV}.tar.gz
#	file:///gentoo/distfiles/systemd-${VERSION_EUDEV}.tar.gz

# 	https://git.kernel.org/pub/scm/linux/kernel/git/dhowells/keyutils.git/snapshot/keyutils-${VERSION_KEYUTILS}.tar.gz

#	https://git.kernel.org/pub/scm/linux/kernel/git/colyli/bcache-tools.git/snapshot/a5e3753516bd39c431def86c8dfec8a9cea1ddd4.tar.gz -> bcache-tools-${VERSION_BCACHE_TOOLS}.tar.gz

COMMON_URI="
	https://github.com/g2p/bcache-tools/archive/399021549984ad27bf4a13ae85e458833fe003d7.tar.gz -> bcache-tools-${VERSION_BCACHE_TOOLS}.tar.gz
	https://boostorg.jfrog.io/artifactory/main/release/${VERSION_BOOST}/source/boost_${VERSION_BOOST//./_}.tar.bz2
	https://www.kernel.org/pub/linux/kernel/people/kdave/btrfs-progs/btrfs-progs-v${VERSION_BTRFS_PROGS}.tar.xz
	https://www.busybox.net/downloads/busybox-${VERSION_BUSYBOX}.tar.bz2
	mirror://gnu/coreutils/coreutils-${VERSION_COREUTILS}.tar.xz
	https://www.kernel.org/pub/linux/utils/cryptsetup/v$(ver_cut 1-2 ${VERSION_CRYPTSETUP})/cryptsetup-${VERSION_CRYPTSETUP}.tar.xz
	https://people.redhat.com/~heinzm/sw/dmraid/src/dmraid-${VERSION_DMRAID}.tar.bz2
	https://matt.ucc.asn.au/dropbear/releases/dropbear-${VERSION_DROPBEAR}.tar.bz2
	https://dev.gentoo.org/~blueness/eudev/eudev-${VERSION_EUDEV}.tar.gz
	https://github.com/libexpat/libexpat/releases/download/R_${VERSION_EXPAT//\./_}/expat-${VERSION_EXPAT}.tar.xz
	https://www.kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v${VERSION_E2FSPROGS}/e2fsprogs-${VERSION_E2FSPROGS}.tar.xz
	https://github.com/libfuse/libfuse/releases/download/fuse-${VERSION_FUSE}/fuse-${VERSION_FUSE}.tar.gz
	mirror://gnupg/gnupg/gnupg-${VERSION_GPG}.tar.bz2
	https://github.com/gentoo/hwids/archive/hwids-${VERSION_HWIDS}.tar.gz
	https://github.com/open-iscsi/open-iscsi/archive/${VERSION_ISCSI}.tar.gz -> open-iscsi-${VERSION_ISCSI}.tar.gz
	https://s3.amazonaws.com/json-c_releases/releases/json-c-${VERSION_JSON_C}.tar.gz
	https://www.kernel.org/pub/linux/utils/kernel/kmod/kmod-${VERSION_KMOD}.tar.xz
	https://releases.pagure.org/libaio/libaio-${VERSION_LIBAIO}.tar.gz
	mirror://gnupg/libgcrypt/libgcrypt-${VERSION_LIBGCRYPT}.tar.bz2
	mirror://gnupg/libgpg-error/libgpg-error-${VERSION_LIBGPGERROR}.tar.bz2
	https://github.com/besser82/libxcrypt/releases/download/v${VERSION_LIBXCRYPT}/libxcrypt-${VERSION_LIBXCRYPT}.tar.xz
	https://mirrors.kernel.org/sourceware/lvm2/LVM2.${VERSION_LVM}.tgz
	https://www.oberhumer.com/opensource/lzo/download/lzo-${VERSION_LZO}.tar.gz
	https://www.kernel.org/pub/linux/utils/raid/mdadm/mdadm-${VERSION_MDADM}.tar.xz
	http://ftp.rpm.org/popt/releases/popt-1.x/popt-${VERSION_POPT}.tar.gz
	https://github.com/strace/strace/releases/download/v${VERSION_STRACE}/strace-${VERSION_STRACE}.tar.xz
	https://github.com/jthornber/thin-provisioning-tools/archive/v${VERSION_THIN_PROVISIONING_TOOLS}.tar.gz -> thin-provisioning-tools-${VERSION_THIN_PROVISIONING_TOOLS}.tar.gz
	https://github.com/rpodgorny/unionfs-fuse/archive/v${VERSION_UNIONFS_FUSE}.tar.gz -> unionfs-fuse-${VERSION_UNIONFS_FUSE}.tar.gz
	https://lttng.org/files/urcu/userspace-rcu-${VERSION_USERSPACE_RCU}.tar.bz2
	https://www.kernel.org/pub/linux/utils/util-linux/v${VERSION_UTIL_LINUX:0:4}/util-linux-${VERSION_UTIL_LINUX}.tar.xz
	https://www.kernel.org/pub/linux/utils/fs/xfs/xfsprogs/xfsprogs-${VERSION_XFSPROGS}.tar.xz
	https://tukaani.org/xz/xz-${VERSION_XZ}.tar.gz
	https://zlib.net/zlib-${VERSION_ZLIB}.tar.gz
	https://github.com/facebook/zstd/archive/v${VERSION_ZSTD}.tar.gz -> zstd-${VERSION_ZSTD}.tar.gz
	https://git.kernel.org/pub/scm/linux/kernel/git/dhowells/keyutils.git/snapshot/keyutils-${VERSION_KEYUTILS}.tar.gz
"

if [[ ${PV} == 9999* ]] || ${USE_GIT} ; then
	EGIT_REPO_URI="https://anongit.gentoo.org/git/proj/${PN}.git"
	#EGIT_REPO_URI="https://github.com/enometh/genkernel/"
	# XXX do not use EGIT_CLONE_TYPE=shallow or version will fail.
	EGIT_BRANCH=madhu-tip
	inherit git-r3
	S="${WORKDIR}/${P}"
	SRC_URI="${COMMON_URI}"
	KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~ia64 ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86"
else
	SRC_URI="https://dev.gentoo.org/~bkohler/dist/${P}.tar.xz
		${COMMON_URI}"
	KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~ia64 ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86"
fi

DESCRIPTION="Gentoo automatic kernel building scripts"
HOMEPAGE="https://wiki.gentoo.org/wiki/Genkernel https://gitweb.gentoo.org/proj/genkernel.git/"

LICENSE="GPL-2"
SLOT="0"
IUSE="ibm +firmware"
REQUIRED_USE="${PYTHON_REQUIRED_USE}"

# Note:
# We need sys-devel/* deps like autoconf or automake at _runtime_
# because genkernel will usually build things like LVM2, cryptsetup,
# mdadm... during initramfs generation which will require these
# things.
DEPEND="
	app-text/asciidoc
"
#;madhu 240912
#echo app-alternatives/yacc-1-r2 > /etc/portage/profile/bison
#echo app-alternatives/lex-0-r1 > /etc/portage/profile/flex

RDEPEND="${PYTHON_DEPS}
	app-alternatives/cpio
	>=app-misc/pax-utils-1.2.2
	app-portage/elt-patches
	app-portage/portage-utils
	dev-util/gperf
	sys-apps/sandbox
	dev-build/autoconf
	dev-build/autoconf-archive
	dev-build/automake
	app-alternatives/bc
	app-alternatives/yacc
	app-alternatives/lex
	dev-build/libtool
	virtual/pkgconfig
	elibc_glibc? ( sys-libs/glibc[static-libs(+)] )
	firmware? ( sys-kernel/linux-firmware )
"

PATCHES=(
)

src_unpack() {
	if [[ ${PV} == 9999* ]] || ${USE_GIT} ; then
		git-r3_src_unpack
	else
		local gk_src_file
		for gk_src_file in ${A} ; do
			if [[ ${gk_src_file} == genkernel-* ]] ; then
				unpack "${gk_src_file}"
			fi
		done
	fi
}

src_prepare() {
	default

	if [[ ${PV} == 9999* ]] || ${USE_GIT} ; then
		einfo "Updating version tag"
		GV="$(git describe --tags | sed 's:^v::')"
		if [ -z "$GV" ]; then
			echo "failed to describe get the version tag. shallow repository?. Using $PV"
			 GV="${PV}"
		fi
		GK_V="${GV}-git"
		sed "/^GK_V/s,=.*,='${GK_V}',g" -i "${S}"/genkernel
		einfo "Producing ChangeLog from Git history..."
		pushd "${S}/.git" >/dev/null || die
		git log > "${S}"/ChangeLog || die
		popd >/dev/null || die
	fi

	# Export all the versions that may be used by genkernel build.
	for v in $(set |awk -F= '/^VERSION_/{print $1}') ; do
	export ${v}
	done

	if use ibm ; then
		cp "${S}"/arch/ppc64/kernel-2.6{-pSeries,} || die
	else
		cp "${S}"/arch/ppc64/kernel-2.6{.g5,} || die
	fi

}

src_compile() {
	emake PREFIX=/usr
}

src_install() {
	emake DESTDIR="${D}" PREFIX=/usr install
	dodoc AUTHORS ChangeLog README TODO

	python_fix_shebang "${ED}"/usr/share/genkernel/path_expander.py

	newbashcomp "${FILESDIR}"/genkernel-4.bash "${PN}"
	insinto /etc
	doins "${FILESDIR}"/initramfs.mounts

	pushd "${DISTDIR}" &>/dev/null || die
	mkdir ${ED}/usr/share/genkernel/distfiles.d/
#;madhu 230803
#	insinto /usr/share/genkernel/distfiles
#	doins ${A/${P}.tar.xz/}
	ls ${A/${P}.tar.xz/} > ${ED}/usr/share/genkernel/distfiles.d/${P}.list
	popd &>/dev/null || die
}

pkg_postinst() {
	# Wiki is out of date
	#echo
	#elog 'Documentation is available in the genkernel manual page'
	#elog 'as well as the following URL:'
	#echo
	#elog 'https://wiki.gentoo.org/wiki/Genkernel'
	#echo

	local replacing_version
	for replacing_version in ${REPLACING_VERSIONS} ; do
		if ver_test "${replacing_version}" -lt 4 ; then
			# This is an upgrade which requires user review

			ewarn ""
			ewarn "Genkernel v4.x is a new major release which touches"
			ewarn "nearly everything. Be careful, read updated manpage"
			ewarn "and pay special attention to program output regarding"
			ewarn "changed kernel command-line parameters!"

			# Show this elog only once
			break
		fi
	done

	if [[ $(find /boot -name 'kernel-genkernel-*' 2>/dev/null | wc -l) -gt 0 ]] ; then
		ewarn ''
		ewarn 'Default kernel filename was changed from "kernel-genkernel-<ARCH>-<KV>"'
		ewarn 'to "vmlinuz-<KV>". Please be aware that due to lexical ordering the'
		ewarn '*default* boot entry in your boot manager could still point to last kernel'
		ewarn 'built with genkernel before that name change, resulting in booting old'
		ewarn 'kernel when not paying attention on boot.'
	fi

	# Show special warning for users depending on remote unlock capabilities
	local gk_config="${EROOT}/etc/genkernel.conf"
	if [[ -f "${gk_config}" ]] ; then
		if grep -q -E "^SSH=[\"\']?yes" "${gk_config}" 2>/dev/null ; then
			if ! grep -q dosshd /proc/cmdline 2>/dev/null ; then
				ewarn ""
				ewarn "IMPORTANT: SSH is currently enabled in your genkernel config"
				ewarn "file (${gk_config}). However, 'dosshd' is missing from current"
				ewarn "kernel command-line. You MUST add 'dosshd' to keep sshd enabled"
				ewarn "in genkernel v4+ initramfs!"
			fi
		fi

		if grep -q -E "^CMD_CALLBACK=.*emerge.*@module-rebuild" "${gk_config}" 2>/dev/null ; then
			elog ""
			elog "Please remove 'emerge @module-rebuild' from genkernel config"
			elog "file (${gk_config}) and make use of new MODULEREBUILD option"
			elog "instead."
		fi
	fi

	local n_root_args=$(grep -o -- '\<root=' /proc/cmdline 2>/dev/null | wc -l)
	if [[ ${n_root_args} -gt 1 ]] ; then
		ewarn "WARNING: Multiple root arguments (root=) on kernel command-line detected!"
		ewarn "If you are appending non-persistent device names to kernel command-line,"
		ewarn "next reboot could fail in case running system and initramfs do not agree"
		ewarn "on detected root device name!"
	fi

	if [[ -d /run ]] ; then
		local permission_run_expected="drwxr-xr-x"
		local permission_run=$(stat -c "%A" /run)
		if [[ "${permission_run}" != "${permission_run_expected}" ]] ; then
			ewarn "Found the following problematic permissions:"
			ewarn ""
			ewarn "    ${permission_run} /run"
			ewarn ""
			ewarn "Expected:"
			ewarn ""
			ewarn "    ${permission_run_expected} /run"
			ewarn ""
			ewarn "This is known to be causing problems for any UDEV-enabled service."
		fi
	fi
}
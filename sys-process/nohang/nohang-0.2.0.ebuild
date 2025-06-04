# Copyright 2019-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Aug 04 10:40:13 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200804 0.1 v0.1-696-gad2ff5d
# ebuild from https://git.sr.ht/~happy_shredder/eph_kit/blob/master/sys-process/nohang/nohang-9999.ebuild
# ;madhu 250604 0.2.0 v0.2.0-19-gbf477da. - ship the tarball in Manifest even of USE_GIT=true, but don't unpack it (expr report: not good), fix logrotate for prefix

EAPI=8

inherit systemd

USE_GIT=true

DESCRIPTION="A sophisticated low memory handler for Linux"
HOMEPAGE="https://github.com/hakavlad/nohang"
LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="systemd"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/hakavlad/nohang"
	EGIT_BRANCH="master"
#	EGIT_BRANCH="dev"
#	EGIT_COMMIT="${PV}"
fi

# else
SRC_URI="https://github.com/hakavlad/nohang/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"
#fi

IUSE="doc systemd"

DEPEND="dev-lang/python
	systemd? ( sys-apps/systemd )
	!systemd? ( sys-apps/openrc )
	"

RDEPEND="${DEPEND}"
BDEPEND="doc? ( app-text/pandoc )"

if ${USE_GIT}; then
	S="${WORKDIR}"/"${P}"
fi

src_unpack() {
	if ${USE_GIT}; then
		git-r3_src_unpack
	else
		if [[ -n ${A} ]]; then
			unpack ${A}
		fi
	fi
}

src_compile() {
	:
}

src_install() {
	local install_args=()

	if use systemd; then
		install_args+=( install )
	else
		install_args+=( -B install-openrc )
	fi

	DOCDIR="${EPREFIX}/usr/share/doc/${P}" \
		  PREFIX="${EPREFIX}/usr" SYSCONFDIR="${EPREFIX}/etc" \
		  emake DESTDIR="${D}" "${install_args[@]}"

	# - fix logrotate makefile oversight
	if ${ED} != ${D}; then
		mv ${D}/etc/logrotate.d ${ED}/etc/logrotate.d
		rmdir ${D}/etc || die like a dog on the highway
	fi

	docompress -x /usr/share/man/
}
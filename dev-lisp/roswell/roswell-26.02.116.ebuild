# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Mar 13 00:02:20 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260313 26.02.116, USE_GIT=true

EAPI=8

USE_GIT=true
MY_EGIT_COMMIT=aa261f4e465e320ec19fb1b8eba6f9f40bbacae9
inherit autotools flag-o-matic

DESCRIPTION="A lisp installer and launcher for major environment"
HOMEPAGE="https://github.com/roswell/roswell"
if ${USE_GIT}; then
   inherit git-r3
   EGIT_REPO_URI="https://github.com/roswell/roswell"

# EGIT_OVERRIDE_REPO_ROSWELL_ROSWELL=file:///build/git-mirror/roswell.git
# EGIT_OVERRIDE_BRANCH_ROSWELL_ROSWELL
# EGIT_OVERRIDE_COMMIT_ROSWELL_ROSWELL
# EGIT_OVERRIDE_COMMIT_DATE_ROSWELL_ROSWELL

   EGIT_CLONE_TYPE=shallow
   EGIT_COMMIT=$MY_EGIT_COMMIT
   EGIT_BRANCH=master
else
	SRC_URI="https://github.com/roswell/roswell/archive/v${PV}.tar.gz -> ${P}.tar.gz"
fi
SLOT="0"
LICENSE="MIT"

KEYWORDS="~amd64 ~x86"

# File collision with librouteros (#691754)
RDEPEND="!net-libs/librouteros
	net-misc/curl"
DEPEND="${RDEPEND}"

src_prepare() {
	default
	eautoreconf
}

src_configure() {
	# -Werror=lto-type-mismatch
	# https://bugs.gentoo.org/856106
	# https://github.com/roswell/roswell/issues/584
	filter-lto

	default
}

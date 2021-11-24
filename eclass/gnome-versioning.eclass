# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sat Jan 16 19:03:32 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210329 40.0 => 40.0
#
#
# 40.alpha => 40_alpha
# 40.alpha.1 => 40_alpha1
# 40.alpha.1.1 => 40_alpha1_p1
#

# @ECLASS: gnome-versioning.eclass
# @MAINTAINER:
# enometh@meer.net
# @BLURB: map gnome-style alpha/beta/rc version names to ebuild names
# @DESCRIPTION
#    include gnome-versioning.eclass when your ebuild targets src_uri
#    tarballs with alpha/beta/rc version names
#
#    | target version number | ebuild name pattern |
#    +-----------------------+---------------------+
#    | 40.alpha              | 40_alpha            |
#    | 40.alpha.1            | 40_alpha1           |
#    | 40.alpha.1.1          | 40_alpha1_p1        |
#
# @EXAMPLE:
# `inherit gnome-versioniong' in
# x11-wm/mutter/mutter-40_alpha1_p1.ebuild would set SRC_URI to
# https://download.gnome.org/sources/mutter/40/mutter-40.alpha.1.1.tar.xz
#
# you have to have a USE_GIT=false before calling inherit

MAJORMINORVER=${PV%%_*}

SUFFIXVER=${PV##${MAJORMINORVER}_}

if  [ ! ${SUFFIXVER} = ${PV} ]; then
SUFFIX1=$(ver_cut 1 "${SUFFIXVER}") #alpha/beta/pre
SUFFIXREST=$(ver_cut 2- "${SUFFIXVER}")
SUFFIXREST=${SUFFIXREST/[a-z]/}
SUFFIXREST=$(ver_rs 1- . "${SUFFIXREST}")
SUFFIX=${SUFFIX1}${SUFFIXREST:+.}$SUFFIXREST
else
SUFFIX= SUFFIXREST=
fi

MY_PV=${MAJORMINORVER}${SUFFIX:+.}${SUFFIX}
MY_P=${PN}-${MY_PV}

MYSUBDIR=$(ver_cut 1 ${PV})

if ! ${USE_GIT}; then
	SRC_URI="https://download.gnome.org/sources/${PN}/${MYSUBDIR}/$PN-${MY_PV}.tar.xz"
	S="${WORKDIR}/${MY_P}"
fi

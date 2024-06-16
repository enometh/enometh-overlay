# Copyright 2020-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Jun 16 08:35:54 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 240616 0 -- uid picked by adduser. acct-user lacks --no-create-home --disabled-login --disabled-password

EAPI=8

inherit acct-user

DESCRIPTION="CouchDB Administrator"
ACCT_USER_ID=100
ACCT_USER_GROUPS=( couchdb )
ACCT_USER_HOME=/opt/couchdb
ACCT_USER_HOME_PERMS=0755
ACCT_USER_SHELL=/bin/bash
acct-user_add_deps
SLOT="0"

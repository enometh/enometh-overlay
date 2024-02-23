# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Feb 23 10:35:17 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 240223 1.07 alternative to libxls, app-text/xslx2csv, ships the excel2cvs script from https://github.com/xevo/xls2csv (not in cpan)

EAPI=8

DIST_AUTHOR=KEN
DIST_VERSION=1.07
inherit perl-module

DESCRIPTION="A script that recodes a spreadsheet's charset and saves as CSV"
SLOT="0"
KEYWORDS="~amd64 ~x86"
RDEPEND="
  dev-perl/libintl-perl
  dev-perl/Unicode-Map
  dev-perl/Spreadsheet-ParseExcel
  dev-perl/Text-CSV
  dev-perl/Spreadsheet-XLSX
  dev-perl/Text-Iconv
"

BDEPEND="${RDEPEND}
	virtual/perl-ExtUtils-MakeMaker
	test? (
		virtual/perl-Test-Simple
	)
"

PATCHES=( ${FILESDIR}/xls2csv-1.07--git-202106-231.diff )

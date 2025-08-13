# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2022-07-05 17:06:32 IST>
#   Touched: Tue Jul 05 15:27:30 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220705 22.1.2 - pypi, no gentoo python baggage (except shell completions)
# ;madhu 24040411 24.0
# ;madhu 250813 25.1.1

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{11..14} pypy3 )
#PYTHON_COMPAT=( "${PYTHON_TESTED[@]}" )
PYTHON_REQ_USE="ssl(+),threads(+)"

inherit  distutils-r1 shell-completion pypi

DESCRIPTION="The PyPA recommended tool for installing Python packages"
HOMEPAGE="
	https://pip.pypa.io/en/stable/
	https://pypi.org/project/pip/
	https://github.com/pypa/pip/
"
# SRC_URI="mirror://pypi/${PN:0:1}/${PN}/${P}.tar.gz"
#SRC_URI="
#	https://github.com/pypa/pip/archive/${PV}.tar.gz -> ${P}.gh.tar.gz
#"
# files.pythonhosted.org/packages/94/59/6638090c25e9bc4ce0c42817b5a234e183872a1129735a9330c472cc2056/pip-24.0.tar.gz /gentoo/distfiles

LICENSE="MIT"
# bundled deps
LICENSE+=" Apache-2.0 BSD BSD-2 ISC LGPL-2.1+ MPL-2.0 PSF-2"

SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="test test-rust"
RESTRICT="!test? ( test )"

PATCHES+=( "${FILESDIR}/pip-20.0.2-disable-system-install.patch" )

RDEPEND="
	>=dev-python/setuptools-70.3.0[${PYTHON_USEDEP}]
"
BDEPEND="
	${RDEPEND}
"

python_compile_all() {
	# 'pip completion' command embeds full $0 into completion script, which confuses
	# 'complete' and causes QA warning when running as "${PYTHON} -m pip".
	# This trick sets correct $0 while still calling just installed pip.
	local pipcmd='if True:
		import sys
		sys.argv[0] = "pip"
		__file__ = ""
		from pip._internal.cli.main import main
		sys.exit(main())
	'
	"${EPYTHON}" -c "${pipcmd}" completion --bash > completion.bash || die
	"${EPYTHON}" -c "${pipcmd}" completion --zsh > completion.zsh || die
}

python_install_all() {
	local DOCS=( AUTHORS.txt docs/html/**/*.rst )
	local HTML_DOCS=( docs/html/. )

	distutils-r1_python_install_all

	newbashcomp completion.bash pip
	newzshcomp completion.zsh _pip

#	insinto /usr/share/zsh/site-functions
#	newins completion.zsh _pip
}

# Copyright 2022-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Jun 03 17:30:15 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250603 1.27 (also in Gentoo, py3.9 as of 2025-05-03)

EAPI=8

DISTUTILS_USE_PEP517=standalone
PYTHON_TESTED=( pypy3_11 python3_{9..13} )
PYTHON_COMPAT=( "${PYTHON_TESTED[@]}" python3_13t )

inherit distutils-r1

TAG=${P/-/-v}
MY_P=hatch-${TAG}
DESCRIPTION="Modern, extensible Python build backend"
HOMEPAGE="
	https://pypi.org/project/hatchling/
	https://github.com/pypa/hatch/
"
SRC_URI="
	https://github.com/pypa/hatch/archive/${TAG}.tar.gz
		-> ${MY_P}.gh.tar.gz
"
S=${WORKDIR}/${MY_P}/backend

LICENSE="MIT"
SLOT="0"
KEYWORDS="~alpha amd64 arm arm64 hppa ~loong ~m68k ~mips ppc ppc64 ~riscv ~s390 sparc x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"
IUSE="test"
RESTRICT="!test? ( test )"

# deps are listed in backend/src/hatchling/ouroboros.py
# editables are optional but required for editable installs
RDEPEND="
	>=dev-python/editables-0.3[${PYTHON_USEDEP}]
	>=dev-python/packaging-24.2[${PYTHON_USEDEP}]
	>=dev-python/pathspec-0.10.1[${PYTHON_USEDEP}]
	>=dev-python/pluggy-1.0.0[${PYTHON_USEDEP}]
	$(python_gen_cond_dep '
		>=dev-python/tomli-1.2.2[${PYTHON_USEDEP}]
	' 3.10)
	dev-python/trove-classifiers[${PYTHON_USEDEP}]
"
BDEPEND="
	${RDEPEND}
	test? (
		$(python_gen_cond_dep '
			dev-python/atomicwrites[${PYTHON_USEDEP}]
			dev-python/click[${PYTHON_USEDEP}]
			dev-python/httpx[${PYTHON_USEDEP}]
			dev-python/platformdirs[${PYTHON_USEDEP}]
			dev-python/pytest[${PYTHON_USEDEP}]
			dev-python/pytest-mock[${PYTHON_USEDEP}]
			dev-python/rich[${PYTHON_USEDEP}]
			dev-python/tomli-w[${PYTHON_USEDEP}]
			dev-python/virtualenv[${PYTHON_USEDEP}]
		' "${PYTHON_TESTED[@]}")
	)
"

src_test() {
	# top-level "tests" directory contains tests both for hatch
	# and hatchling
	cd "${WORKDIR}/${MY_P}" || die

	# sigh; standalone test suites are overrated, right?
	echo "__version__ = '${PV}'" > src/hatch/_version.py || die
	local -x PYTHONPATH="src:${PYTHONPATH}"

	# do not require uv for portability, sigh
	mkdir "${T}/bin" || die
	cat > "${T}/bin/uv" <<-EOF || die
		#!/bin/sh
		exit 127
	EOF
	chmod +x "${T}/bin/uv" || die
	local -x PATH=${T}/bin:${PATH}

	# tests mock cargo subprocess call but the backend raises if CARGO
	# is not set and shutil.which() can't find it
	local -x CARGO=cargo

	distutils-r1_src_test
}

python_test() {
	if ! has "${EPYTHON}" "${PYTHON_TESTED[@]/_/.}"; then
		einfo "Skipping tests on ${EPYTHON}"
		return
	fi

	local -x EPYTEST_DESELECT=(
		# most of these run uv to install stuff
		# the few remaining tests aren't worth running
		tests/backend/dep/test_core.py
		# broken if CARGO is set
		tests/backend/builders/test_binary.py::TestBuildBootstrap::test_no_cargo
	)

	local -x PYTEST_DISABLE_PLUGIN_AUTOLOAD=1
	epytest -p pytest_mock tests/backend
}
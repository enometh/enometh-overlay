# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Oct 20 17:20:51 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 211020 3.7.0 -> 3.8.0-13-gcbace11 from GIT. does not work
# without git. 3.8.0-15-gb2c6893
# ;madhu 220327 3.8.5 3.8.5-4-g2081945
# ;madhu 221024 3.9.0 3.9.0-22-ga294664
# ;madhu 240607 3.9.10 3.10-9-gbb76160  (misnamed)
# ;madhu 250710 3.10.0 3.10-26-g2197650

EAPI=8

USE_GIT=true

DISTUTILS_SINGLE_IMPL=1
DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{10..13} )
inherit distutils-r1 gnome2-utils optfeature virtualx xdg # plocale

#PLOCALES="ca cs de el es fa fi fr gl hr hu id it ja ko nb nl pa pl pt_BR ru sv tr uk zh_CN zh_TW"

if [[ ${PV} == *9999 ]] || ${USE_GIT} ; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/Guake/guake.git"

	# XXX
	EGIT_COMMIT=bb76160dc1ccc3147567f323bfcf63c4ebffbe58
	EGIT_CLONE_TYPE=shallow
else
	SRC_URI="https://github.com/Guake/guake/archive/refs/tags/${PV}.tar.gz -> ${P}.gh.tar.gz"
#	SRC_URI="mirror://pypi/${PN:0:1}/${PN}/${P}.tar.gz"
fi

DESCRIPTION="Drop-down terminal for GNOME"
HOMEPAGE="https://guake.github.io/"

LICENSE="GPL-2+"
SLOT="0"
KEYWORDS="amd64 arm ~arm64 ~ppc64 x86"
IUSE="utempter"

RDEPEND="
	$(python_gen_cond_dep '
		dev-python/dbus-python[${PYTHON_USEDEP}]
		dev-python/pycairo[${PYTHON_USEDEP}]
		dev-python/pygobject:3[${PYTHON_USEDEP}]
		dev-python/pyyaml[${PYTHON_USEDEP}]')
	dev-libs/keybinder:3[introspection]
	x11-libs/libnotify[introspection]
	x11-libs/libwnck:3[introspection]
	x11-libs/vte:2.91[introspection]
	utempter? ( sys-libs/libutempter )"
BDEPEND="
	$(python_gen_cond_dep '
		dev-python/setuptools-scm[${PYTHON_USEDEP}]
		test? (
			dev-python/pyfakefs[${PYTHON_USEDEP}]
			dev-python/pytest-mock[${PYTHON_USEDEP}]
		)')
	dev-libs/glib:2
	gnome-base/gsettings-desktop-schemas
	sys-devel/gettext"

PATCHES=(
${FILESDIR}/guake-3.9.0-terminal.py-encode-uname-to-bytes-for-utempter.patch
${FILESDIR}/guake-3.10.0-prefs.py-PrefsDialog.load_configs-set-font-lev.patch
)

distutils_enable_tests pytest

src_prepare() {
	echo > po/LINGUAS
	find po -iname "*.po" -exec rm -fv '{}' \;
	distutils-r1_src_prepare

#if ! ${USE_GIT}; then
#	export PBR_VERSION=${PV} # needed if using github's tarball
	export SETUPTOOLS_SCM_PRETEND_VERSION=${PV} # needed with github's tarball
#fi

	local po=($(plocale_get_locales disabled))
#	po=("${po[@]/%/.po}")
#	(( ! ${#po[@]} )) || rm "${po[@]/#/po/}" || die

	emake PREFIX="${EPREFIX}"/usr prepare-install # paths.py.in -> paths.py
}

python_test() {
	# - uses /usr/bin/bash if SHELL is not exported
	SHELL=${SHELL} virtx epytest
}

python_install() {
	# use right schema/data dirs, done here so tests don't use the system's
	sed -e "/^SCHEMA_DIR/s|=.*|= \"${EPREFIX}/usr/share/glib-2.0/schemas\"|" \
		-e "/def get_default_data_dir/{n;s|=.*|= \"${EPREFIX}/usr/share/guake\"|}" \
		-i "${BUILD_DIR}/install$(python_get_sitedir)"/guake/paths.py || die
	python_optimize "${BUILD_DIR}/install$(python_get_sitedir)"/guake/paths.py

	distutils-r1_python_install
}

python_install_all() {
	emake DESTDIR="${D}" PREFIX="${EPREFIX}"/usr install-{locale,schemas}

	dodoc NEWS.rst README.rst
}

pkg_postinst() {
	xdg_pkg_postinst
	gnome2_schemas_update

	optfeature "utmp management support" sys-libs/libutempter
}

pkg_postrm() {
	xdg_pkg_postrm
	gnome2_schemas_update
}

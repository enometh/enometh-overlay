# @ECLASS: distutils-r2-legacy.eclass
# @MAINTAINER:
# Python team <python@gentoo.org>
# @AUTHOR:
# Author: Michał Górny <mgorny@gentoo.org>
# @SUPPORTED_EAPIS: 7 8
# @PROVIDES: python-r1 python-single-r1
# @BLURB@: anathema! maranatha!

# @ECLASS_VARIABLE: DISTUTILS_USE_PEP517
# @PRE_INHERIT
# @DEFAULT_UNSET
# @DESCRIPTION:
# Enable the PEP517 mode for the specified build system.  In this mode,
# the complete build and install is done in python_compile(),
# a venv-style install tree is provided to python_test(),
# and python_install() just merges the temporary install tree
# into the real fs.
#
# This mode is recommended for Python packages.  However, some packages
# using custom hacks on top of distutils/setuptools may not install
# correctly in this mode.  Please verify the list of installed files
# when using it.
#
# The variable specifies the build system used.  Currently,
# the following values are supported:
#
# - flit - flit-core backend
#
# - flit_scm - flit_scm backend
#
# - hatchling - hatchling backend (from hatch)
#
# - jupyter - jupyter_packaging backend
#
# - maturin - maturin backend
#
# - meson-python - meson-python (mesonpy) backend
#
# - no - no PEP517 build system (see below)
#
# - pbr - pbr backend
#
# - pdm-backend - pdm.backend backend
#
# - poetry - poetry-core backend
#
# - scikit-build-core - scikit-build-core backend
#
# - setuptools - distutils or setuptools (incl. legacy mode)
#
# - sip - sipbuild backend
#
# - standalone - standalone/local build systems
#
# - uv-build - uv-build backend (using dev-python/uv)
#
# The variable needs to be set before the inherit line.  If another
# value than "standalone" and "no" is used, The eclass adds appropriate
# build-time dependencies, verifies the value and calls the appropriate
# modern entry point for the backend.  With DISTUTILS_UPSTREAM_PEP517,
# this variable can be used to override the upstream build backend.
#
# The value of "standalone" indicates that upstream is using a custom,
# local build backend.  In this mode, the eclass does not add any
# dependencies, disables build backend verification and uses the exact
# entry point listed in pyproject.toml.
#
# The special value "no" indicates that the package has no build system.
# This is not equivalent to unset DISTUTILS_USE_PEP517 (legacy mode).
# It causes the eclass not to include any build system dependencies
# and to disable default python_compile() and python_install()
# implementations.  Baseline Python deps and phase functions will still
# be set (depending on the value of DISTUTILS_OPTIONAL).  Most of
# the other eclass functions will work.  Testing venv will be provided
# in ${BUILD_DIR}/install after python_compile(), and if any (other)
# files are found in ${BUILD_DIR}/install after python_install(), they
# will be merged into ${D}.


# @ECLASS_VARIABLE: DISTUTILS_USE_SETUPTOOLS
# @DEFAULT_UNSET
# @PRE_INHERIT
# @DESCRIPTION:
# Controls adding dev-python/setuptools dependency.  The allowed values
# are:
#
# - no -- do not add the dependency (pure distutils package)
#
# - bdepend -- add it to BDEPEND (the default)
#
# - rdepend -- add it to BDEPEND+RDEPEND (e.g. when using pkg_resources)
#
# - manual -- do not add the dependency and suppress the checks
#   (assumes you will take care of doing it correctly)
#
# This variable is effective only if DISTUTILS_OPTIONAL is disabled.
# It is available only in non-PEP517 mode.  It needs to be set before
# the inherit line.


# @FUNCTION: _distutils-r1_create_setup_cfg
# @INTERNAL
# @DESCRIPTION:
# Create implementation-specific configuration file for distutils,
# setting proper build-dir (and install-dir) paths.
_distutils-r1_create_setup_cfg() {
	if [[ ${DISTUTILS_USE_PEP517} ]]; then
		die "${FUNCNAME} is not implemented in PEP517 mode"
	fi

	cat > "${HOME}"/.pydistutils.cfg <<-_EOF_ || die
		[build]
		build_base = ${BUILD_DIR}

		# using a single directory for them helps us export
		# ${PYTHONPATH} and ebuilds find the sources independently
		# of whether the package installs extensions or not
		#
		# note: due to some packages (wxpython) relying on separate
		# platlib & purelib dirs, we do not set --build-lib (which
		# can not be overridden with --build-*lib)
		build_platlib = %(build_base)s/lib
		build_purelib = %(build_base)s/lib

		# make the ebuild writer lives easier
		build_scripts = %(build_base)s/scripts

		# this is needed by distutils_install_for_testing since
		# setuptools like to create .egg files for install --home.
		[bdist_egg]
		dist_dir = ${BUILD_DIR}/dist

		# avoid packing up eggs in a zip as it often breaks test suites
		[options]
		zip_safe = False
	_EOF_

	if [[ ${EBUILD_PHASE} == install ]]; then
		# we can't refer to ${D} before src_install()
		cat >> "${HOME}"/.pydistutils.cfg <<-_EOF_ || die

			# installation paths -- allow calling extra install targets
			# without the default 'install'
			[install]
			compile = True
			optimize = 2
			root = ${D}
		_EOF_

		if [[ ! ${DISTUTILS_SINGLE_IMPL} ]]; then
			# this gets appended to [install]
			cat >> "${HOME}"/.pydistutils.cfg <<-_EOF_ || die
				install_scripts = $(python_get_scriptdir)
			_EOF_
		fi
	fi
}

# @FUNCTION: _distutils-r1_copy_egg_info
# @INTERNAL
# @DESCRIPTION:
# Copy egg-info files to the ${BUILD_DIR} (that's going to become
# egg-base in esetup.py). This way, we respect whatever's in upstream
# egg-info.
_distutils-r1_copy_egg_info() {
	if [[ ${DISTUTILS_USE_PEP517} ]]; then
		die "${FUNCNAME} is not implemented in PEP517 mode"
	fi

	mkdir -p "${BUILD_DIR}" || die
	# stupid freebsd can't do 'cp -t ${BUILD_DIR} {} +'
	find -name '*.egg-info' -type d -exec cp -R -p {} "${BUILD_DIR}"/ ';' || die
}


# @FUNCTION: _distutils-r1_clean_egg_info
# @INTERNAL
# @DESCRIPTION:
# Clean up potential stray egg-info files left by setuptools test phase.
# Those files ended up being unversioned, and caused issues:
# https://bugs.gentoo.org/534058
_distutils-r1_clean_egg_info() {
	if [[ ${DISTUTILS_USE_PEP517} ]]; then
		die "${FUNCNAME} is not implemented in PEP517 mode"
	fi

	rm -rf "${BUILD_DIR}"/lib/*.egg-info || die
}

# @FUNCTION: _distutils-r1_post_python_test
# @INTERNAL
# @DESCRIPTION:
# Post-phase function called after python_test.
_distutils-r1_post_python_test() {
	debug-print-function ${FUNCNAME} "$@"

	if [[ ! ${DISTUTILS_USE_PEP517} ]]; then
		_distutils-r1_clean_egg_info
	fi
}

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# @FUNCTION: distutils_install_for_testing
# @DEPRECATED: DISTUTILS_USE_PEP517=...
# @DESCRIPTION:
# This function used to provide an installed package for running tests.
# It is no longer implemented, PEP517 mode must be used instead.
distutils_install_for_testing() {
	die "${FUNCNAME} has been removed, please use PEP517 mode instead"
}


# @FUNCTION: _distutils-r1_disable_ez_setup
# @INTERNAL
# @DESCRIPTION:
# Stub out ez_setup.py and distribute_setup.py to prevent packages
# from trying to download a local copy of setuptools.
_distutils-r1_disable_ez_setup() {
	if [[ ${DISTUTILS_USE_PEP517} ]]; then
		die "${FUNCNAME} is not implemented in PEP517 mode"
	fi

	local stub="def use_setuptools(*args, **kwargs): pass"
	if [[ -f ez_setup.py ]]; then
		echo "${stub}" > ez_setup.py || die
	fi
	if [[ -f distribute_setup.py ]]; then
		echo "${stub}" > distribute_setup.py || die
	fi
}

# @FUNCTION: _distutils-r1_handle_pyproject_toml
# @INTERNAL
# @DESCRIPTION:
# Verify whether DISTUTILS_USE_SETUPTOOLS is set correctly
# for pyproject.toml build systems (in non-PEP517 mode).
_distutils-r1_handle_pyproject_toml() {
	if [[ ${DISTUTILS_USE_PEP517} ]]; then
		die "${FUNCNAME} is not implemented in PEP517 mode"
	fi

	[[ ${DISTUTILS_USE_SETUPTOOLS} == manual ]] && return

	if [[ ! -f setup.py && -f pyproject.toml ]]; then
		eerror "No setup.py found but pyproject.toml is present.  Please migrate"
		eerror "the package to use DISTUTILS_USE_PEP517. See:"
		eerror "  https://projects.gentoo.org/python/guide/distutils.html"
		die "No setup.py found and PEP517 mode not enabled"
	fi
}


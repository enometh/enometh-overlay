# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Jul 27 08:39:42 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020 Madhu.  All Rights Reserved.
#
# ;madhu 200727 rgv-overlay 2.36 -> 4.15
#
# ;madhu 210101 4.22 EGO_SUM extracted from go.sum. go-module.eclass
# calls go mod tidy it barfs on unneded deps, and leaves unwriteable
# directories under ${PORTAGE_BUILDDIR}/homedir which cannot be
# removed. Fix that in src_unpack
#
# ;madhu 230118 4.33 go-difflib causes an error in go mod tidy which is
# run in src_unpack (it should run in src_prepare) to give us a chance
# to remove the offending test file
# github.com/ianbruene/go-difflib@v1.2.0/difflib/difflib_test.go which
# includes a relative import.
#

EAPI="8"

inherit go-module

# echo "EGO_SUM=(" ; awk '{print $1 " " $2}' go.sum | sed -e 's/^/   "/g' -e 's/$/"/g'; echo ")"
EGO_SUM=(
   "github.com/anmitsu/go-shlex v0.0.0-20200514113438-38f4b401e2be"
   "github.com/anmitsu/go-shlex v0.0.0-20200514113438-38f4b401e2be/go.mod"
#   "github.com/chzyer/logex v1.1.10"
   "github.com/chzyer/logex v1.1.10/go.mod"
   "github.com/chzyer/readline v0.0.0-20180603132655-2972be24d48e"
   "github.com/chzyer/readline v0.0.0-20180603132655-2972be24d48e/go.mod"
#   "github.com/chzyer/test v0.0.0-20180213035817-a1ea475d72b1"
   "github.com/chzyer/test v0.0.0-20180213035817-a1ea475d72b1/go.mod"
   "github.com/emirpasic/gods v1.12.0"
   "github.com/emirpasic/gods v1.12.0/go.mod"
   "github.com/ianbruene/go-difflib v1.2.0"
   "github.com/ianbruene/go-difflib v1.2.0/go.mod"
   "github.com/kballard/go-shellquote v0.0.0-20180428030007-95032a82bc51"
   "github.com/kballard/go-shellquote v0.0.0-20180428030007-95032a82bc51/go.mod"
   "github.com/pkg/term v1.1.0"
   "github.com/pkg/term v1.1.0/go.mod"
   "github.com/termie/go-shutil v0.0.0-20140729215957-bcacb06fecae"
   "github.com/termie/go-shutil v0.0.0-20140729215957-bcacb06fecae/go.mod"
   "github.com/xo/terminfo v0.0.0-20210125001918-ca9a967f8778"
   "github.com/xo/terminfo v0.0.0-20210125001918-ca9a967f8778/go.mod"
   "gitlab.com/esr/fqme v0.1.0"
   "gitlab.com/esr/fqme v0.1.0/go.mod"
   "gitlab.com/ianbruene/kommandant v0.6.2"
   "gitlab.com/ianbruene/kommandant v0.6.2/go.mod"
   "golang.org/x/sys v0.0.0-20200909081042-eff7692f9009/go.mod"
   "golang.org/x/sys v0.0.0-20210615035016-665e8c7367d1/go.mod"
   "golang.org/x/sys v0.0.0-20220114195835-da31bd327af9"
   "golang.org/x/sys v0.0.0-20220114195835-da31bd327af9/go.mod"
   "golang.org/x/term v0.0.0-20210927222741-03fcf44c2211"
   "golang.org/x/term v0.0.0-20210927222741-03fcf44c2211/go.mod"
   "golang.org/x/text v0.3.7"
   "golang.org/x/text v0.3.7/go.mod"
   "golang.org/x/tools v0.0.0-20180917221912-90fa682c2a6e/go.mod"
)
go-module_set_globals

DESCRIPTION="a tool for editing version-control repository history"
HOMEPAGE="http://catb.org/~esr/reposurgeon/"
SRC_URI="http://www.catb.org/~esr/${PN}/${P}.tar.xz
	${EGO_SUM_SRC_URI}"

LICENSE="BSD-2" #BOGUS
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""

RDEPEND=""
DEPEND="${RDEPEND}
	virtual/pkgconfig
	app-text/xmlto
	app-text/asciidoc
	dev-ruby/asciidoctor"

#DOCS=( README.md CONFIGURATION.md INTRODUCTION.md LICENSE LICENSE.pycrc LICENSE.snappy )

PATCHES=(
	"${FILESDIR}/${P}-Makefile-install.patch"
)

src_unpack() {
	go-module_src_unpack
	find ${HOME} -type d \! -perm 755 -print0 | xargs -0 chmod -v 755
}

src_compile() {
	emake all
}

src_install() {
	emake  prefix=${EPREFIX}/usr DESTDIR=${ED} install
}
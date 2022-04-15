# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#
#   Time-stamp: <>
#   Touched: Thu Apr 14 19:14:47 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220410 1.27.1  v1.27.1-3-g5aec41b

EAPI=8

USE_GIT=true

USE_GIT=true
MY_COMMIT=5aec41b4809b9822a34e17acd06ae9ae9f41c13d

inherit  go-module

DESCRIPTION="Go support for Protocol Buffers: protoc-gen-go"

HOMEPAGE="https://pkg.go.dev/google.golang.org/protobuf
https://github.com/protocolbuffers/protobuf-go/releases"

EGO_SUM=(
	"github.com/golang/protobuf v1.5.0"
	"github.com/golang/protobuf v1.5.0/go.mod"
	"github.com/google/go-cmp v0.5.5"
	"github.com/google/go-cmp v0.5.5/go.mod"
	"golang.org/x/xerrors v0.0.0-20191204190536-9bdfabe68543"
	"golang.org/x/xerrors v0.0.0-20191204190536-9bdfabe68543/go.mod"
	"google.golang.org/protobuf v1.26.0-rc.1/go.mod"
	)

go-module_set_globals

if ${USE_GIT}; then
	inherit git-r3
	SRC_URI="${EGO_SUM_SRC_URI}"
	EGIT_REPO_URI="https://github.com/protocolbuffers/protobuf-go"
	#EGIT_OVERRIDE_REPO_PROTOCOLBUFFERS_PROTOBUF_GO=file:///build/git-mirror/protobuf-go.git
	EGIT_COMMIT=$MY_COMMIT
	EGIT_CLONE_TYPE=shallow
else
	SRC_URI="https://github.com/protocolbuffers/protobuf-go/archive/refs/tags/v{PV}.zip"
	SRC_URI+=" ${EGO_SUM_SRC_URI}"
fi

LICENSE="BSD MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"

RESTRICT="mirror test"

src_unpack() {
	if ${USE_GIT} ; then
		git-r3_src_unpack
	else
		default
	fi
	go-module_setup_proxy
}

src_compile() {
	pushd cmd/protoc-gen-go
	go build -v -x || die
	popd
}

src_install(){
	exeinto /usr/bin
	doexe ${S}/cmd/protoc-gen-go/protoc-gen-go
}

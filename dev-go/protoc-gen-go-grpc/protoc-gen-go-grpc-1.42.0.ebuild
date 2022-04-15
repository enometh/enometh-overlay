# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Apr 14 19:14:47 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 220410 1.42.0 v1.42.0-dev-11-g11437f66

EAPI=8

USE_GIT=true
MY_COMMIT=11437f66f20f3473e09fcf3fb5c23d4388af936f

inherit  go-module

DESCRIPTION="Go support for Protocol Buffers: protoc-gen-go"
HOMEPAGE="https://github.com/grpc/grpc-go/releases"

EGO_SUM=(
	"github.com/golang/protobuf v1.4.0-rc.1/go.mod"
	"github.com/golang/protobuf v1.4.0-rc.1.0.20200221234624-67d41d38c208/go.mod"
	"github.com/golang/protobuf v1.4.0-rc.2/go.mod"
	"github.com/golang/protobuf v1.4.0-rc.4.0.20200313231945-b860323f09d0/go.mod"
	"github.com/golang/protobuf v1.4.0/go.mod"
	"github.com/google/go-cmp v0.3.0/go.mod"
	"github.com/google/go-cmp v0.3.1/go.mod"
	"github.com/google/go-cmp v0.4.0"
	"github.com/google/go-cmp v0.4.0/go.mod"
	"golang.org/x/xerrors v0.0.0-20191204190536-9bdfabe68543"
	"golang.org/x/xerrors v0.0.0-20191204190536-9bdfabe68543/go.mod"
	"google.golang.org/protobuf v0.0.0-20200109180630-ec00e32a8dfd/go.mod"
	"google.golang.org/protobuf v0.0.0-20200221191635-4d8936d0db64/go.mod"
	"google.golang.org/protobuf v0.0.0-20200228230310-ab0ca4ff8a60/go.mod"
	"google.golang.org/protobuf v1.20.1-0.20200309200217-e05f789c0967/go.mod"
	"google.golang.org/protobuf v1.21.0/go.mod"
	"google.golang.org/protobuf v1.23.0"
	"google.golang.org/protobuf v1.23.0/go.mod"
	)

go-module_set_globals

if ${USE_GIT}; then
	inherit git-r3
	SRC_URI="${EGO_SUM_SRC_URI}"
	EGIT_REPO_URI="https://github.com/grpc/grpc-go"
	#EGIT_OVERRIDE_REPO_GRPC_GRPC_GO="file:///build/git-mirror/grpc-go.git"
	EGIT_COMMIT=$MY_COMMIT
	EGIT_CLONE_TYPE=shallow
else
	SRC_URI="https://github.com/grpc/grpc-go/archive/refs/tags/v{PV}.tar.gz"
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
	pushd cmd/protoc-gen-go-grpc
	go build -v -x || die
	popd
}

src_install(){
	exeinto /usr/bin
	doexe ${S}/cmd/protoc-gen-go-grpc/protoc-gen-go-grpc
}

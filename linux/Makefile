TREE?=HEAD
ARTIFACTS=`pwd`/artifacts
REVISION?=1

build:
	mkdir -p $(ARTIFACTS)
	docker build -t alpine-pandoc .
	docker run --env TREE=$(TREE) --env REVISION=$(REVISION) \
	    -v $(ARTIFACTS):/artifacts alpine-pandoc

setup:
	docker pull alpine:edge

.PHONY: build setup

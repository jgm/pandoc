TREE?=HEAD
ARTIFACTS=`pwd`/artifacts

build:
	mkdir -p $(ARTIFACTS)
	docker build -t alpine-pandoc .
	docker run --env TREE=$(TREE) -v $(ARTIFACTS):/artifacts alpine-pandoc

setup:
	docker pull alpine:edge

.PHONY: build setup

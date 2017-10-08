TREE?=HEAD
ARTIFACTS=`pwd`/artifacts
REVISION?=1

build:
	mkdir -p $(ARTIFACTS)
	docker build -t alpine-pandoc .
	docker run --env TREE=$(TREE) --env REVISION=$(REVISION) \
	    -v $(ARTIFACTS):/artifacts alpine-pandoc

interact:
	docker run --env TREE=$(TREE) --env REVISION=$(REVISION) \
		-v $(ARTIFACTS):/artifacts -it alpine-pandoc bash

setup:
	docker pull alpine:edge

.PHONY: build setup interact

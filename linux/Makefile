TREE?=HEAD
ARTIFACTS=`pwd`/artifacts
REVISION?=1

build:
	mkdir -p $(ARTIFACTS)
	docker build -t debian-pandoc .
	docker run --env TREE=$(TREE) --env REVISION=$(REVISION) \
	    -v $(ARTIFACTS):/artifacts debian-pandoc

interact:
	docker run --env TREE=$(TREE) --env REVISION=$(REVISION) \
		-v $(ARTIFACTS):/artifacts -it debian-pandoc bash

setup:
	docker pull debian:jessie

.PHONY: build setup interact

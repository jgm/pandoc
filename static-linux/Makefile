ARTIFACTS=`pwd`/artifacts

build:
	mkdir -p $(ARTIFACTS)
	docker run -v $(ARTIFACTS):/artifacts alpine-pandoc

setup:
	docker pull alpine:edge
	docker build -t alpine-pandoc .

.PHONY: build setup

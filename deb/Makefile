COMMIT?=HEAD

.PHONY: package

package:
	vagrant up
	vagrant ssh -c 'rm -rf pandoc && git clone https://github.com/jgm/pandoc && cd pandoc && git checkout -b work $(COMMIT) && git submodule update --init && ./make_deb.sh && cp *.deb /vagrant_data/'
	vagrant halt

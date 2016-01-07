TREE?=HEAD
DEBPKGVER?=1
VAGRANTBOX?=debian/wheezy64

.PHONY: package clean

package:
	VAGRANTBOX=$(VAGRANTBOX) vagrant up
	vagrant ssh -c 'rm -rf pandoc && git clone https://github.com/jgm/pandoc && cd pandoc && git checkout -b work $(TREE) && git submodule update --init && DEBPKGVER=$(DEBPKGVER) sh -ev ./deb/make_deb.sh && cp *.deb /vagrant_data/'
	vagrant halt

clean:
	vagrant destroy

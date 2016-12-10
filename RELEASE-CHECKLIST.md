- [ ] Test, on linux, windows, mac (inc. website demos)

- [ ] Finalize changelog:
   `git log --pretty='format:%n%n* %s (%an)%n%b%n%h%n' --reverse --name-only 1.17.0.3..HEAD > LOG`

- [ ] `make man/pandoc.1` and commit if needed

- [ ] Tag release in git

- [ ] Tag templates

- [ ] Generate Windows package (`make winpkg`)

- [ ] Generate Mac OSX package (`make osxpkg`)

- [ ] Generate Ubuntu/Debian deb package (`make debpkg`)

- [ ] Add release on github (use `make changes_github` and upload files)

- [ ] Upload to HackageDB

- [ ] Update website (`make update`), including short description of changes (`make changes`)

- [ ] on server, `cabal install --enable-tests -ftrypandoc`
  and then `cd trypandoc; sudo make install`

- [ ] Announce on pandoc-announce, pandoc-discuss

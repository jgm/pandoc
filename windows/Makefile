version?=$(shell grep '^[Vv]ersion:' ../pandoc.cabal | awk '{print $$2;}')

pandoc-$(version)-windows-%.msi: pandoc-windows-%-UNSIGNED.msi
	osslsigncode sign -pkcs12 $$HOME/Private/SectigoCodeSigning.exp2023.p12 -in $< -i http://johnmacfarlane.net/ -t http://timestamp.comodoca.com/ -out $@ -askpass


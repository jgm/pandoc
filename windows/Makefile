UNSIGNED=$(wildcard pandoc-*-UNSIGNED.msi)
SIGNED=$(patsubst %-UNSIGNED.msi, %.msi, $(UNSIGNED))

all: $(SIGNED)

.PHONY: all

pandoc-%.msi: pandoc-%-UNSIGNED.msi
	osslsigncode sign -pkcs12 $$HOME/Private/SectigoCodeSigning.exp2026.pfx -in $< -i https://johnmacfarlane.net/ -t http://timestamp.comodoca.com/ -out $@ -askpass && rm $<


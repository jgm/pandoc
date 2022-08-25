upload:
	scp trypandoc.js examples.js website:pandoc.org/try/
	scp index.html website:pandoc.org/try/index.html.in
	ssh website 'sed -e "s/__DATE__/$$(date -Iseconds)/" pandoc.org/try/index.html.in > pandoc.org/try/index.html'

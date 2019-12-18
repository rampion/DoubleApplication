slides: README.html

build:
	cabal v2-build

README.html: README.md reveal.js
	pandoc -t revealjs -s -o README.html README.md -V revealjs-url=./reveal.js

reveal.js:
	curl --location -O https://github.com/hakimel/reveal.js/archive/master.tar.gz | tar xz
	mv reveal.js-master reveal.js


repl:
	cabal v2-repl --repl-options=-XRank2Types --repl-options=-XConstraintKinds --repl-options=-XAllowAmbiguousTypes --repl-options=-XFlexibleInstances --repl-options=-XTypeApplications --repl-options=-XScopedTypeVariables --repl-options=-XTypeOperators --repl-options=-XPolyKinds --repl-options=-XDataKinds --repl-options=-XTypeInType --repl-options=-XTypeFamilies --repl-options=-XUndecidableInstances

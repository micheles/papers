decorators.rst: decorators.py $S/minidoc.py
	$S/minidoc.py -r decorators -r
decorators.html: decorators.txt decorators.png decorators.rst 
	$S/rst.py decorators.html
decorators.dvi: decorators.txt decorators.rst 
	$S/rst.py -t decorators.tex
	perl -pi -e 's/\(non-strict/\n(non-strict/' decorators.tex
	perl -pi -e 's/as first/\nas first/' decorators.tex
	$S/rst.py -d decorators.dvi
decorators.pdf: decorators.dvi decorators.ps
	dvipdf decorators
decorators.ps: makegraph.dot
	dot makegraph.dot -Tps -o decorators.ps -Gsize="5.5,6"
decorators.png: makegraph.dot
	dot makegraph.dot -Tpng -o decorators.png -Gsize="5.5,6"
decorators.zip: decorators.html decorators.pdf \
	decorators.py README.txt $S/safetype.py $S/doct.py 
	zip -j decorators decorators.py decorators.png decorators.ps \
	decorators.html decorators.txt decorators.pdf decorators.rst\
	README.txt makegraph.dot $S/safetype.py $S/doct.py
dist: decorators.zip
working: decorators.zip
	unzip -o decorators.zip -d working

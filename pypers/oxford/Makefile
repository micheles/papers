rst = "/home/micheles/mp/ms/tools/rst.py"
all:
	make all.rst
all.rst: frontpage.txt loops.txt objects.txt magic.txt 
	$(rst) frontpage.txt loops.txt objects.txt magic.txt
pdf: all.rst
	$(rst) -Ptd all; xpdf all.pdf
test: all.rst
	python2.4 -mdoctester < all.rst
zip: all.rst
	zip oxford-lectures README.txt all.rst all.html all.pdf accu2005.png \
	doctester.py
upload:
	scp oxford-lectures.zip alpha.phyast.pitt.edu:public_html

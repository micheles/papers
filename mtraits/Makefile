RST = python ~/trunk/ROnline/RCommon/Python/ms/tools/rst.py
MINIDOC = python ~/trunk/ROnline/RCommon/Python/ms/tools/minidoc.py

upload: docs.py strait.py
	python setup.py register build sdist upload
docs: docs.py
	$(MINIDOC) -dH docs; scp /tmp/docs.html merlin.phyast.pitt.edu:public_html/python/strait.html; $(RST) -tp /tmp/docs.rst; mv /tmp/docs.pdf .

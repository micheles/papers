upload: docs.py strait.py
	python -m ms.tools.minidoc -dp docs
	mv /tmp/docs.pdf .
	python setup.py register build sdist upload

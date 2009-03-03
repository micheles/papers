upload:
	python make_sweet_macros.py
	zip -r aps aps/README.txt sweet-macros/main.sls sweet-macros/main.mzscheme.sls sweet-macros/helper1.mzscheme.sls sweet-macros/helper2.mzscheme.sls sweet-macros/helper3.mzscheme.sls aps/compat.ikarus.sls aps/compat.ypsilon.sls aps/compat.mzscheme.sls aps/cut.sls aps/easy-test.sls aps/list-utils.sls aps/repeat.sls
	scp aps.zip merlin.phyast.pitt.edu:public_html/scheme

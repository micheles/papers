# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         = a4

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d /tmp/build/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

.PHONY: help clean html web pickle htmlhelp latex changes linkcheck

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html      to make standalone HTML files"
	@echo "  pickle    to make pickle files"
	@echo "  json      to make JSON files"
	@echo "  htmlhelp  to make HTML files and a HTML help project"
	@echo "  latex     to make LaTeX files, you can set PAPER=a4 or PAPER=letter"
	@echo "  changes   to make an overview over all changed/added/deprecated items"
	@echo "  linkcheck to check all external links for integrity"

clean:
	-rm -rf /tmp/build/*

html:
	mkdir -p /tmp/build/html /tmp/build/doctrees
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) /tmp/build/html
	@echo
	@echo "Build finished. The HTML pages are in /tmp/build/html."

pickle:
	mkdir -p /tmp/build/pickle /tmp/build/doctrees
	$(SPHINXBUILD) -b pickle $(ALLSPHINXOPTS) /tmp/build/pickle
	@echo
	@echo "Build finished; now you can process the pickle files."

web: pickle

json:
	mkdir -p /tmp/build/json /tmp/build/doctrees
	$(SPHINXBUILD) -b json $(ALLSPHINXOPTS) /tmp/build/json
	@echo
	@echo "Build finished; now you can process the JSON files."

htmlhelp:
	mkdir -p /tmp/build/htmlhelp /tmp/build/doctrees
	$(SPHINXBUILD) -b htmlhelp $(ALLSPHINXOPTS) /tmp/build/htmlhelp
	@echo
	@echo "Build finished; now you can run HTML Help Workshop with the" \
	      ".hhp project file in /tmp/build/htmlhelp."

latex:
	mkdir -p /tmp/build/latex /tmp/build/doctrees
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) /tmp/build/latex
	@echo
	@echo "Build finished; the LaTeX files are in /tmp/build/latex."
	@echo "Run \`make all-pdf' or \`make all-ps' in that directory to" \
	      "run these through (pdf)latex."

changes:
	mkdir -p /tmp/build/changes /tmp/build/doctrees
	$(SPHINXBUILD) -b changes $(ALLSPHINXOPTS) /tmp/build/changes
	@echo
	@echo "The overview file is in /tmp/build/changes."

linkcheck:
	mkdir -p /tmp/build/linkcheck /tmp/build/doctrees
	$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) /tmp/build/linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in /tmp/build/linkcheck/output.txt."

POST = python ../post.py
RST = python ../../scheme2rst.py -r

1: scheme1.ss
	$(RST) scheme1.ss; $(POST) scheme1.rst 238789

2: scheme2.ss
	$(RST) scheme2.ss; $(POST) scheme2.rst 238941

3: scheme3.ss
	$(RST) scheme3.ss; $(POST) scheme3.rst 239466

4: scheme4.ss
	$(RST) scheme4.ss; $(POST) scheme4.rst 239568

5: scheme5.ss
	$(RST) scheme5.ss; $(POST) scheme5.rst 239699

6: scheme6.ss
	$(RST) scheme6.ss; $(POST) scheme6.rst 240198

second-cycle: second-cycle.txt
	$(POST) second-cycle.txt 240789

7: scheme7.ss
	$(RST) scheme7.ss; $(POST) scheme7.rst 240781

8: scheme8.ss
	$(RST) scheme8.ss; $(POST) scheme8.rst 240793

9: scheme9.ss
	$(RST) scheme9.ss; $(POST) scheme9.rst 240804

10: scheme10.ss
	$(RST) scheme10.ss; $(POST) scheme10.rst 240805

11: scheme11.ss
	$(RST) scheme11.ss; $(POST) scheme11.rst 240833

12: scheme12.ss
	$(RST) scheme12.ss; $(POST) scheme12.rst 240836

third-cycle: third-cycle.txt
	$(POST) third-cycle.txt 243830

13: scheme13.ss
	$(RST) scheme13.ss; $(POST) scheme13.rst 248953

14: scheme14.ss
	$(RST) scheme14.ss; $(POST) scheme14.rst 249198

15: scheme15.ss
	$(RST) scheme15.ss; $(POST) scheme15.rst 249681

16: scheme16.ss
	$(RST) scheme16.ss; $(POST) scheme16.rst 249685

17: scheme17.ss
	$(RST) scheme17.ss; $(POST) scheme17.rst 251156

18: scheme18.ss
	$(RST) scheme18.ss; $(POST) scheme18.rst 251159

table-of-contents: table-of-contents.txt
	$(POST) table-of-contents.txt 251474

fourth-cycle: fourth-cycle.txt
	$(POST) fourth-cycle.txt 253071

eurolisp2: eurolisp2.txt
	$(POST) eurolisp2.txt 259100

19: scheme19.ss
	$(RST) scheme19.ss; $(POST) scheme19.rst 251476

20: scheme20.ss
	$(RST) scheme20.ss; $(POST) scheme20.rst 255303

21: scheme21.ss
	$(RST) scheme21.ss; $(POST) scheme21.rst 255612

22: scheme22.ss
	$(RST) scheme22.ss; $(POST) scheme22.rst 256848

23: scheme23.ss
	$(RST) scheme23.ss; $(POST) scheme23.rst 257998

24: scheme24.ss
	$(RST) scheme24.ss; $(POST) scheme24.rst 258103

25: scheme25.ss
	$(RST) scheme25.ss; $(POST) scheme25.rst 258580

26: scheme26.ss
	$(RST) scheme26.ss; $(POST) scheme26.rst 259977

27: scheme27.ss
	$(RST) scheme27.ss; $(POST) scheme27.rst 260182

28: scheme28.ss
	$(RST) scheme28.ss; $(POST) scheme28.rst 260195

29: scheme29.ss
	$(RST) scheme29.ss; $(POST) scheme29.rst 261363

30: scheme30.ss
	$(RST) scheme30.ss; $(POST) scheme30.rst 261364

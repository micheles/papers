POST = python ../post.py
MINIDOC = python ~/trunk/ROnline/RCommon/Python/ms/tools/minidoc.py

default:
	echo "Available python files:"; ls *.py

super1:	super1.py
	$(MINIDOC) -d super1; $(POST) /tmp/super1.rst 236275

super2:	super2.py
	$(MINIDOC) -d super2; $(POST) /tmp/super2.rst 236278

super3:	super3.py
	$(MINIDOC) -d super3; $(POST) /tmp/super3.rst 237121

generic: genericfunctions.py
	$(MINIDOC) -d genericfunctions; $(POST) /tmp/genericfunctions.rst 237764

epp: expert-python-programming.txt
	python2.5 ../post.py expert-python-programming.txt 240415

records1: records1.py
	$(MINIDOC) -d records1; $(POST) /tmp/records1.rst 236637

decorator3: decorator3.txt
	$(POST) decorator3.txt 243843

decorator3-is-out: decorator3-is-out.txt
	$(POST) decorator3-is-out.txt 245050

mixins1: mixins1.py
	$(MINIDOC) -d mixins1; $(POST) /tmp/mixins1.rst 246341

mixins2: mixins2.py
	$(MINIDOC) -d mixins2; $(POST) /tmp/mixins2.rst 246483

traits: traits.txt
	$(POST) traits.txt 246488

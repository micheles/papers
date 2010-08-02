POST = python ../post.py
RST = python ~/trunk/ROnline/RCommon/Python/ms/tools/rst.py
MINIDOC = python ~/trunk/ROnline/RCommon/Python/ms/tools/minidoc.py

default:
	echo "Available python files:"; ls *.py

super1:	super1.py
	$(MINIDOC) -d super1; $(POST) /tmp/super1.rst 236275

super2:	super2.py
	$(MINIDOC) -d super2; $(POST) /tmp/super2.rst 236278

super3:	super3.py
	$(MINIDOC) -d super3; $(POST) /tmp/super3.rst 237121

super4:	super4.py
	$(MINIDOC) -d super4; $(POST) /tmp/super4.rst 281127

super: super.rst super1.py super2.py super3.py super4.py
	$(RST) -tp super.rst

generic: genericfunctions.py
	$(MINIDOC) -d genericfunctions; $(POST) /tmp/genericfunctions.rst 237764

epp: expert-python-programming.txt
	python2.5 ../post.py expert-python-programming.txt 240415

pycon3: pycon3.txt
	python2.5 ../post.py pycon3.txt 248750

records1: records1.py
	$(MINIDOC) -d records1; $(POST) /tmp/records1.rst 236637

records2: records2.py
	$(MINIDOC) -d records2; $(POST) /tmp/records2.rst 269269

records3: records3.py
	$(MINIDOC) -d records3; $(POST) /tmp/records3.rst 

decorator3: decorator3.txt
	$(POST) decorator3.txt 243843

decorator3-is-out: decorator3-is-out.txt
	$(POST) decorator3-is-out.txt 245050

mixins1: mixins1.py
	$(MINIDOC) -d mixins1; $(POST) /tmp/mixins1.rst 246341

mixins2: mixins2.py
	$(MINIDOC) -d mixins2; $(POST) /tmp/mixins2.rst 246483

mixins3: mixins3.py
	$(MINIDOC) -d mixins3; $(POST) /tmp/mixins3.rst 254367

mixins4: mixins4.py
	$(MINIDOC) -d mixins4; $(POST) /tmp/mixins4.rst 254507

traits: traits.txt
	$(POST) traits.txt 246488

caches: caches.py
	$(MINIDOC) -d caches; $(POST) /tmp/caches.rst 274438

parallel: parallel.txt
	$(POST) parallel.txt 299551

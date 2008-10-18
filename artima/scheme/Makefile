POST = python2.5 ../post.py
RST = python2.5 ~/md/scripts/scheme2rst.py -r

default:
	echo "Available python files:"; ls *.py

scheme1: scheme1.txt
	$(POST) scheme1.txt 238789

scheme2: scheme2.txt
	$(POST) scheme2.txt 238941

scheme3: scheme3.txt
	$(POST) scheme3.txt 239466

scheme4: scheme4.txt
	$(POST) scheme4.txt 239568

scheme5: scheme5.txt
	$(POST) scheme5.txt 239699

scheme6: scheme6.txt
	$(POST) scheme6.txt 240198

second-cycle: second-cycle.txt
	$(POST) second-cycle.txt 240789

scheme7: scheme7.ss
	$(RST) scheme7.ss; $(POST) scheme7.rst 240781

scheme8: scheme8.ss
	$(RST) scheme8.ss; $(POST) scheme8.rst 240793

scheme9: scheme9.ss
	$(RST) scheme9.ss; $(POST) scheme9.rst 240804

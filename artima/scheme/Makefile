POST = python2.5 ../post.py
RST = python2.5 ~/md/scripts/scheme2rst.py -r

default:
	echo "Available python files:"; ls *.py

1: scheme1.txt
	$(POST) scheme1.txt 238789

2: scheme2.txt
	$(POST) scheme2.txt 238941

3: scheme3.txt
	$(POST) scheme3.txt 239466

4: scheme4.txt
	$(POST) scheme4.txt 239568

5: scheme5.txt
	$(POST) scheme5.txt 239699

6: scheme6.txt
	$(POST) scheme6.txt 240198

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
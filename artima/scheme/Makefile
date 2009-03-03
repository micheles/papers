POST = python2.5 ../post.py
RST = python2.5 ../../scheme2rst.py -r

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

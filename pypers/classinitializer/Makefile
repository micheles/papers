RST = $(MS)/tools/rst.py

all: classinitializer.txt
	python doctester.py classinitializer.txt
	$(RST) classinitializer
	$(RST) -Pt classinitializer
	zip classinitializer classinitializer.txt classinitializer.html \
		classinitializer.pdf doctester.py _main.py
upload:
	scp classinitializer.html classinitializer.zip \
            micheles@alpha.phyast.pitt.edu:public_html/python

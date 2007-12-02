default:
	make allchapters
preface.html: preface.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets preface.txt preface.html
first.html: first.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets first.txt first.html
functions.html: functions.txt	
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets functions.txt functions.html
objects.html: objects.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets objects.txt objects.html
classes.html: classes.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets classes.txt classes.html
descr.html: descr.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets descr.txt descr.html
MI.html: MI.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets MI.txt MI.html
meta.html: meta.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets meta.txt meta.html
secret.html: secret.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets secret.txt secret.html
magic.html: magic.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets magic.txt magic.html
prog.html: prog.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets prog.txt prog.html
app1.html: app1.txt
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets app1.txt app1.html

html: 
	python2 /home/micheles/packages/docutils/tools/html.py \
	--footnote-references=brackets all.txt all.html

ps: preface.html first.html functions.html objects.html classes.html \
	descr.html MI.html meta.html magic.html secret.html prog.html app1.html
	htmldoc preface.html first.html functions.html objects.html \
	classes.html descr.html MI.html meta.html magic.html secret.html \
        prog.html app1.html -f OOPP.ps

pdf: *.html
	htmldoc preface.html first.html functions.html objects.html \
	classes.html descr.html MI.html meta.html magic.html secret.html \
	prog.html app1.html -f OOPP.pdf

allchapters: preface.html first.html functions.html objects.html classes.html \
	descr.html MI.html meta.html magic.html secret.html prog.html app1.html
	make ps; make pdf; gv OOPP.ps&

dist: preface.txt first.txt functions.txt objects.txt classes.txt descr.txt \
	meta.txt magic.txt secret.txt prog.txt app1.txt README.txt \
        EXECUTEME.py test.py
	zip dist preface.txt first.txt functions.txt objects.txt classes.txt \
	descr.txt meta.txt magic.txt secret.txt prog.txt app1.txt README.txt \
	EXECUTEME.py test.py

all.txt: preface.txt first.txt functions.txt objects.txt classes.txt descr.txt\
	meta.txt magic.txt secret.txt prog.txt app1.txt
	/home/micheles/md/scripts/cat.py preface.txt first.txt functions.txt\
	objects.txt classes.txt descr.txt MI.txt meta.txt magic.txt secret.txt\
	prog.txt app1.txt > all.txt

all: all.txt
	/home/micheles/md/scripts/rst.py all.txt
	htmldoc all.html -f OOPP.ps
	gv OOPP.ps	 

all.tex: all.txt
	/home/micheles/md/scripts/rst.py -tbx all

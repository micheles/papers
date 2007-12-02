"""Check the  Python scripts contained in the OOPP tutorial.
In addition, it automatically create the module oopp containing
many useful routines.

Notes
-----

1. A very cryptic error message of this kind

Traceback (most recent call last):
  File "test.py", line 82, in ?
    if __name__=='__main__': main()
  File "test.py", line 74, in main
    check(chapter,scriptname,code); n=n+1
  File "test.py", line 62, in check
    open(scriptname,'w').write(code)
IOError: invalid argument: w

means that some of your text files is DOS style. Working solution:

$dos2unix *.txt
---

This script is 1.5.2 backward compatible.
exec and execfile have a bug in 2.2: executed code does not recognize
correctly descriptors! It would work running in a separate interpreter.

"""

#check Python version
import sys,os,string
version=string.split(sys.version)[0]
msg=('\nSorry, your current Python version is %s.\n' % version+
     'You must use Python 2.2+ to run this program.\n')
if version <'2.2': raise SystemExit('*'*45+msg+'*'*45)

#open files for append
file("output.txt","w").close() #erase previous content
sys.stdout=file("output.txt","a") #append mode
file("oopp.py","w").close() #erase previous content
ooppmodule=file("oopp.py","a") #append mode

#global lists
err=[]; arglist=sys.argv[1:]

def extract(chapter,condition):
    """This function collects interpreter lines, oopp lines and script lines
    and returns them is a list of string tuples with the form
    [(chaptername,scriptname,code)]"""
    code=[]; ooppls=[]; codels=[]; inter=[]
    for line in file(chapter):
        l=line.lstrip()
        if ((l.startswith('>>>') or l.startswith('...'))
            and not (l.endswith('error\n') or l.endswith('.\n')) ): 
            inter.append(line[6:]) #valid interprer line
        elif l.startswith('#</oopp'): #end oopp routine
            ooppmodule.write(''.join(ooppls)+'\n')
            ooppls=[]
        elif l.startswith('#<oopp'):#start oopp routine
            ooppls=['']
        elif ooppls: #read oopp routine lines
            ooppls.append(line[2:])
        elif l.startswith('#</'): #end script
            if condition(scriptname):
                codels.append((chapter,scriptname,''.join(code)))
            code=[]
        elif l.startswith('#<'): #start script
            scriptname=l[2:-2]
            code=['']
        elif code: #read script lines
            code.append(line[2:])
    #Add all the lines of interactive code in this chapter
    icode=''.join(inter) # interactive code
    iname=chapter[:-4]+'_inter.py'
    if icode: codels.append((chapter,iname,icode))
    return codels

def check(chapter,scriptname,code):
    # write the code in scriptname
    # print >> sys.stderr,scriptname
    f=open(scriptname,'w'); f.write(code); f.close()
    print '\n'+'='*77+'\n'
    print '--- Script "%s"' % scriptname,'in',chapter,':\n\n'+code
    print '--- Output of "%s":\n' % scriptname; error=0 # default
    inter=scriptname.endswith('inter.py')
    if inter: sys.stderr.write('-')
    else: sys.stderr.write('+')
    try:
        #os.system('%s %s > /dev/null' % (sys.executable,scriptname))#very safe
        namespace={}; execfile(scriptname,namespace)
    except Exception,e:
        err.append('Error found in %s [%s]:\n  %s' %
                   (scriptname,chapter,e)); error=1
    else:
        error=0
    if not arglist and not error: os.remove(scriptname)

def main():
    if arglist: #check only the scripts in the arglist
        condition=lambda scriptname: (scriptname in arglist)
    else: #check all scripts
        condition=lambda scriptname: 1 
    print "CHECK OF THE SCRIPTS IN 'OBJECT ORIENTED PROGRAMMING IN PYTHON'"
    chapters="preface.txt first.txt functions.txt objects.txt classes.txt "\
           "descr.txt MI.txt meta.txt magic.txt secret.txt prog.txt app1.txt"\
           .split()
    codels=[]; n=0
    for chapter in chapters: # first pass; creates oopp.py
        #if not arglist: sys.stderr.write('\nChecking %s ...\n' % chapter)
        codels = codels + extract(chapter,condition)
    ooppmodule.close() # needed !
    for chapter,scriptname,code in codels:
        check(chapter,scriptname,code)
        if not scriptname.endswith('inter.py'): n=n+1
    if err:
        sys.stderr.write('\n'+'\n'.join(err))
        sys.stderr.write('\nThe debug is left to you.\n')
    else:
        ok='\nGood: executed %s examples with success.\n' % n
        sys.stderr.write(ok+'The output can be found in file output.txt\n')

if __name__=='__main__': main()

"Upload the files which have been modified less than twelve hours ago"

import os,time

pscp=r"C:\WINDOWS\Desktop\Internet\pscp -pw IjrumHrG "
alpha=" micheles@alpha.phyast.pitt.edu:public_html/python/"

reftime=time.time()-3600*12 # twelve hours ago

def copy_to(host,fname):
    os.rename(fname,fname.lower())
    os.system(pscp+'"'+fname+'"'+host)

cd="d:\\mydocs\\pypers\\mro\\"

for f in os.listdir(cd):
    modtime=os.stat(f).st_mtime
    if modtime>reftime and not f.endswith('.py'): copy_to(alpha,cd+f)


err=file('err','w')

def printerr(*args):
    "For debugging purposes"
    for a in args: print >> err, a,
    print >> err
          

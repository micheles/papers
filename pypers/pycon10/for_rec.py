def loop123():
    for i in 1, 2, 3:
        # .....
        print i
        # .....

def loop123(i=1):
    if i > 3:
        return
    else:
        # .....
        print i
        # .....
        loop123(i+1)

loop123()

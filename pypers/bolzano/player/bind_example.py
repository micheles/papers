from Tkinter import *

root = Tk()

def callback(event):
    print "clicked at", event.x, event.y 

def quit(event):
    root.quit()
    
lab = Label(root, bg="red")
lab.bind("<1>", quit)
lab.pack()

root.mainloop()

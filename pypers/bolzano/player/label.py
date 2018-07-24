from Tkinter import *
from Tkconstants import *

fg, bg = "red", "green"

def simple_label(master):
    mylabel = Label(master, text="ciao!", fg="red", font="Courier 36 bold",
                    bg="green")
    mylabel.pack(side=LEFT)

def flashing_label(master): 
    label = Label(master, text="ciao!",
                    foreground=fg, background=bg,
                    font="Courier 36 bold")
    label.pack(side=RIGHT)
    label.after(1000, flash, label)

def flash(mylabel):
    global fg, bg
    fg, bg = bg, fg
    mylabel.config(foreground=fg, background=bg)
    mylabel.after(1000, flash, mylabel)

    
if __name__ == "__main__":
    root = Tk()
    simple_label(root)
    flashing_label(root)
    root.mainloop()

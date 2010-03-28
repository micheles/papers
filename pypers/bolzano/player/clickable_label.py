from Tkinter import *

class ClickableLabel(Label):
    def __init__(self, master, **kw):
        Label.__init__(self, master, **kw)
        self.bind('<Button-1>', lambda e : self.quit())
                  
if __name__ == "__main__":
    root = Tk()
    cl = ClickableLabel(root)
    cl.pack()
    root.mainloop()

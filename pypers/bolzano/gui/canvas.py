from Tkinter import *

def on_click(event):
    print event
    print event.x, event.y
    
root = Tk()

c = Canvas()

c.config(background="red", selectforeground="green")
c.create_rectangle(0, 0, 100, 100)
c.create_line(0, 0, 10, 100, 200, 300)

c.bind("<Button-1>", on_click)
c.pack()

mainloop()

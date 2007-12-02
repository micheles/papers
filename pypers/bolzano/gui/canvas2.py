from Tkinter import *

def on_keypress(event):
    print dir(event)

    
root = Tk()

c = Canvas(takefocus=1)

c.config(background="red", selectforeground="green")
c.create_rectangle(0, 0, 100, 100)
c.create_line(0, 0, 10, 100, 200, 300)

c.bind("<KeyPress>", on_keypress)
c.focus()
c.pack()

mainloop()

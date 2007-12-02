from Tkinter import *

class Output(Label):
    def printkey(self, event):
        self.config(text=event.keysym)

root = Tk()
label = Label(root, text='Press a key...')
output = Output(root, takefocus=1)
label.pack()
output.pack()
output.focus()
output.bind('<KeyPress>', output.printkey)
root.mainloop()


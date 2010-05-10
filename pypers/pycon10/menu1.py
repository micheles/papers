import Tkinter
root = Tkinter.Tk()
text = Tkinter.StringVar()
label = Tkinter.Label(root, textvariable=text)
menubar = Tkinter.Menu(root)
menu = Tkinter.Menu(menubar)
menubar.add_cascade(label='File', menu=menu)
for item in ['F1','F2', 'F3']:
    def showmenu():
        text.set(text.get() + '\n%s' % item)
    menu.add_command(label=item, command=showmenu)
root.config(menu=menubar); label.pack()
root.mainloop()

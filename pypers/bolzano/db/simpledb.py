import os, shelve

from Tkinter import *
from Tkconstants import *

class NameNumber(Frame):
    def __init__(self, db, name, number, row, **kw):
        Frame.__init__(self, db.root, **kw)
        self.namelabel = Entry(db.root)
        self.phonelabel = Entry(db.root)
        
        def delete_key(name=name): db.del_data(name)
        
        self.deletebutton = Button(db.root, text="del", command=delete_key)
        self.namelabel.insert(END, name)
        self.phonelabel.insert(END, number)
        self.namelabel.grid(row=row, column=0)
        self.phonelabel.grid(row=row, column=1)
        self.deletebutton.grid(row=row, column=2)
        
class SimpleDatabase(object):
    def __init__(self):
        self.root = Tk()
    def read_data(self, datafile):
        self.phone = shelve.open(datafile)
    def read_text(self, datafile):
        self.phone = shelve.open(datafile[:-4])
        for line in file(datafile):
            if not line.strip(): continue
            name, number = line[:18].strip(), line[18:-1]
            self.phone[name] = number
    def change_data(self, key):
        pass
    def add_data(self, key):
        pass
    def del_data(self, key):
        del self.phone[key]
        i = self.entrydict[key]
        del self.entrywidget[i]
        
    def commit(self):
        for entry in self.entrywidget:
            name = entry.namelabel.get()
            number = entry.phonelabel.get()
            self.phone[name] = number
        self.phone.sync()
        print "Committed!"
        
    def show_data(self):
        row = 0
        self.entrywidget = []
        self.entrydict = {}
        for name in self.phone:
            namenumber = NameNumber(self, name, self.phone[name], row)
            namenumber.grid()
            self.entrywidget.append(namenumber)
            self.entrydict[name]=row
            row += 1

        self.commit_button = Button(
            self.root,
            text = "Commit",
            command = self.commit)
        self.commit_button.grid()
    
if __name__ == "__main__":
    phonebook = SimpleDatabase()
    # phonebook.read_text("nomi.txt")
    phonebook.read_data("nomi")
    phonebook.show_data()
    phonebook.root.mainloop()

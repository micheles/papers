import os
from Tkinter import *
from call_mpg123 import play
from signal import SIGTERM

class Musicbox(Listbox):

    def __init__(self, master, mp3s, **kw):
        Listbox.__init__(self, master, **kw)
        self.master = master
        self.mp3s = mp3s
        for mp3 in mp3s:
            self.insert("end", mp3)
        self.is_playing = False
        self.pid = None
        self.bind("<Double-Button-1>", self.playsong)

    def playsong(self, e):
        i = int(self.curselection()[0])
        if not self.is_playing:
            self.is_playing = True
            self.pid = play(self.mp3s[i])
        else:
            os.kill(self.pid, SIGTERM)
            self.is_playing = True
            self.pid = play(self.mp3s[i])

    def quit(self):
        self.master.quit()
        if self.is_playing:
            os.kill(self.pid, SIGTERM)

class ScrollableMusicbox(Frame):
    def __init__(self, master, musicbox, **kw):
        Frame.__init__(self, master, **kw)
        self.master = master
        self.musicbox = musicbox
        self.scrollbar = Scrollbar(self)
        self.scrollbar.config(
            command=self.musicbox.yview)
        self.musicbox.config(
            yscrollcommand=self.scrollbar.set)
        self.musicbox.pack(side=LEFT, fill=BOTH)
        self.scrollbar.pack(side=RIGHT, fill=Y)
        

if __name__ == "__main__":
    master = Tk()
    musicbox = Musicbox(master, [f for f in os.listdir(".")
                                 if f.endswith(".mp3")])
    musicbox.pack()
    master.protocol("WM_DELETE_WINDOW", musicbox.quit)
    master.mainloop()

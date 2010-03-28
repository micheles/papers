import os
from Tkinter import *
from call_mpg123 import play
from signal import SIGTERM

mp3s = [f for f in os.listdir(".") if f.endswith(".mp3")]

is_playing = False
pid = None

def playsong(e):
    global is_playing, pid
    i = int(listbox.curselection()[0])
    if not is_playing:
        is_playing = True
        pid = play(mp3s[i])
    else:
        os.kill(pid, SIGTERM)
        is_playing = True
        pid = play(mp3s[i])

def quit():
    master.quit()
    if is_playing:
        os.kill(pid, SIGTERM)
  
master = Tk()
listbox = Listbox(master)
        
for mp3 in mp3s:
    listbox.insert('end', mp3)

listbox.pack()
listbox.bind("<Double-Button-1>", playsong)

master.protocol("WM_DELETE_WINDOW", quit)
master.mainloop()

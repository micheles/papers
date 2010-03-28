import os
from Tkinter import *
from Tkconstants import *
from animated_text2 import AnimatedLabel
from LabelWithImages import LabelWithImages
from musicbox import Musicbox, ScrollableMusicbox

def get_photoimages(directory):
    for fname in os.listdir(directory):
        if fname.endswith(".gif"):
            yield PhotoImage(file=fname)


root = Tk()
label = LabelWithImages(root,
                        images = get_photoimages("."),
                        height = 100)
label.start_animation()
label.pack()

a1 = AnimatedLabel(root, text = "Anime", fg="red")
a2 = AnimatedLabel(root, text = "Music", fg="green")
a1.pack()
a2.pack()

a1.start_animation()
a2.start_animation()
musicbox = Musicbox(
    root, [f for f in os.listdir(".") if f.endswith(".mp3")])
musicframe = ScrollableMusicbox(root, musicbox)

musicframe.pack()

exit_label = Label(text = "Click here to exit")
exit_label.bind("<Button-1>", lambda e: musicbox.quit())
exit_label.pack(side=BOTTOM)

root.protocol("WM_DELETE_WINDOW", musicbox.quit)
root.mainloop()


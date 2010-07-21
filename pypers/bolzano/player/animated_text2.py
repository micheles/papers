from Tkinter import *

class Spacer(object):
    nspaces = 0
    action = "increment"
    MAXSPACES = 10
    def manage_spaces(self):
        if self.action == "increment":
            self.nspaces += 1
        elif self.action == "decrement":
            self.nspaces -= 1
        if self.nspaces > self.MAXSPACES:
            self.action = "decrement"
        if self.nspaces < 0:
            self.action = "increment"
        return " " * self.nspaces

spacer = Spacer()

class AnimatedLabel(Label):
    DELTA_T = 100
    def __init__(self, master, **kw):
        Label.__init__(self, master, **kw)
        self.master = master
        self.word = self.cget("text")
        
    def start_animation(self):
        spaces = spacer.manage_spaces()
        centered_word = spaces.join(self.word).center(50)
        self.config(text=centered_word)
        self.master.after(self.DELTA_T, self.start_animation)

if __name__ == "__main__":
    root = Tk()
    lab = AnimatedLabel(root, text="Anime")
    lab.pack()
    lab.start_animation()
    root.mainloop()

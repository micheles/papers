from Tkinter import *
word = "Anime"
DELTA_T = 100

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

def make_animation(label):
    spaces = spacer.manage_spaces()
    centered_word = spaces.join(word).center(50)
    label.config(text=centered_word)
    root.after(DELTA_T, make_animation, label)

if __name__ == "__main__":
    root = Tk()
    lab = Label(root)
    lab.pack()
    root.after(DELTA_T, make_animation, lab)
    root.mainloop()

from Tkinter import *
from itertools import cycle

class LabelWithImages(Label):
    
    def __init__(self, master, images, **kw):
        Label.__init__(self, master, **kw)
        self.images = cycle(images)
        
    def start_animation(self):
        self.config(image = self.images.next())
        self.after(2000, self.start_animation)
                   


from quixote_utils import RootDirectory
from quixote.util import htmltext
from quixote.html import href
import os

imagedir = "/home/micheles/Desktop/photos-images/images"

class ImageViewer(RootDirectory):
    _q_exports = ["", "prev", "next"]
    def __init__(self, images):
        self.images = images
        self.current_image = 0

    def prev(self):
        self.current_image  -= 1
        return self._q_index()
        
    def next(self):
        self.current_image  += 1
        return self._q_index()
        
    def _q_index(self):
        name = self.images[self.current_image]
        return '''<html><head></head><body>
        <a href="prev">prev</a>
        <a href="next">next</a><br/>
        Current image: %s <br/>
        <img src="file://%s">
        </body><html>
        ''' % (self.current_image, os.path.join(imagedir, name))

viewer = ImageViewer([img for img in os.listdir(imagedir)
                      if img.endswith(".jpg")])

viewer.publish_show(browser="mozilla")

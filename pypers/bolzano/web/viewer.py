from quixote_utils import RootDirectory
from quixote.util import htmltext
from quixote.html import href
import os

imagedir = "/mnt/sda1/photos-images/images"

class ImageViewer(RootDirectory):
    _q_exports = [""]
    def __init__(self, images):
        self.images = images
        self.links = []
        for name in self.images:
            nospaces_name = name.replace(" ", "_")
            self._q_exports.append(nospaces_name)
            self.links.append(href(nospaces_name, name))
            def imagesrc(name=name):
                return "<image src='file://%s'>" % \
                    os.path.join(imagedir, name)
            setattr(self, nospaces_name, imagesrc)
    def _q_index(self):
        return htmltext("<h1>Available images</h1>")+ \
               htmltext("<br/>").join(self.links)

viewer = ImageViewer([img for img in os.listdir(imagedir)
                      if img.endswith(".jpg")])

viewer.publish_show()

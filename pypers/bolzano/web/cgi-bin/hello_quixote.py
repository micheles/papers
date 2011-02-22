from quixote_utils import RootDirectory

class BiancheriaOnLine(RootDirectory):
    _q_exports = ["", "magliette", "maglioni", "calzini", "mutande"]
    
    def _q_index(self):
        return "Benvenuti al nostro negozio di biancheria online!!"
    
    def magliette(self):
        return "magliette"

    def maglioni(self):
        return "maglioni"

    def calzini(self):
        return "calzini"

    def mutande(self):
        return "mutande"

    def oxe(self):
        return "XXX"

if __name__ == "__main__":
    BiancheriaOnLine().publish_show()

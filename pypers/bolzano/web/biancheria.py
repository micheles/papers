from quixote_utils import RootDirectory
from quixote.form.widget import StringWidget, SubmitWidget, CheckboxWidget, \
     RadiobuttonsWidget
from quixote.form.form import Form
from quixote.util import htmltext

class BiancheriaOnLine(RootDirectory):
    _q_exports = ["", "magliette", "maglioni", "calzini", "mutande"]
    
    def _q_index(self):
        return "Benvenuti al nostro negozio di biancheria online!!"
    
    def magliette(self):
        return "<h1>magliette</h1>"

    def maglioni(self):
        return "maglioni"

    def calzini(self):
        msg = htmltext("""
        Abbiamo disponili i seguenti modelli: <br/> <br/>
        A. Calzettone lungo da neve <br/>
        B. Calzino di seta estivo <br/>
        C. Calzino nero lutto <br/>
        """)
        form = Form()
        form.add(RadiobuttonsWidget, "calzini", options=list("ABC"))
        form.add(SubmitWidget, "submit", "Compra!")
        return msg  + form.render()

    def mutande(self):
        msg = htmltext("""
        Abbiamo disponili i seguenti modelli: <br/> <br/>
        A. mutanda modello base <br/>
        B. mutandina di pizzo per signore <br/>
        C. mutandone ascellare modello Fantozzi <br/>
        """)
        form = Form()
        form.add(CheckboxWidget, "modelloA", value=0)
        form.add(CheckboxWidget, "modelloB", value=0)
        form.add(CheckboxWidget, "modelloC", value=1)
        form.add(SubmitWidget, "submit", "Compra!")
        return msg  + form.render()

    def oxe(self):
        return "XXX"

        

if __name__ == "__main__":
    BiancheriaOnLine().publish_show("calzini", browser="mozilla")

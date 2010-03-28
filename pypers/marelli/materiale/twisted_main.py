# twisted_main.py

"Il main che chiama proc2a.py e proc2b.py nell'ordine e gestisce gli errori"

import webbrowser, sys
if sys.platform == "win32":
    from twisted.internet import win32eventreactor
    win32eventreactor.install()

from twisted.internet.utils import getProcessOutput
from twisted.internet import reactor

def scrivi_messaggio(err):
    print err.getErrorMessage()
    reactor.stop()
    import pdb; pdb.set_trace() # fa partire il debugger in caso di errore

def visualizza_histo(out_di_genera_histo):
    print out_di_genera_histo
    webbrowser.open("histo.png")

def genera_histo(out_di_genera_dati):
    print out_di_genera_dati
    getProcessOutput(sys.executable, (r"c:\corso\processi\proc2b.py",)) \
                    .addCallback(visualizza_histo) \
                    .addErrback(scrivi_messaggio)
    
def genera_dati():
    getProcessOutput(sys.executable, (r"c:\corso\processi\proc2a.py",)) \
                     .addCallback(genera_histo) \
                     .addErrback(scrivi_messaggio)

if __name__ == "__main__":
    reactor.callLater(0, genera_dati) # call "genera_dati" after 0 seconds
    reactor.run()
    


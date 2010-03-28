#############################################################################################################
# FILE: TestLauncher.py                                                                                    
# AUTHOR: Alberto Mantovani
# DATE: 16.07.2003
# DESCRIPTION: Interfaccia grafica di lancio test
#            - individua i file descrittori dei TD
#            - crea l'interfaccia grafica relativa alle informazioni trovate
#            - lancia gli script che a loro volta lanciano i TD
#            - visualizza il risultato dei singoli TC
#
# REQUIREMENTS: file .txt descrittori dei TD
# Ultimo Identificativo per pulsanti usato=156 (devono essere esclusi gli identificativi terminanti con 0 1 2)
#                                                                                                          
# MODIFY                                      
# AUTHOR: Alberto Mantovani                                                                    
# DATE: 12.08.2003
# DESCRIPTION: Al lancio dell'eseguibile viene creato un file path.py che contiene la costante LauncherPath
# con il percorso appunto del launcher.
# DATE: 03.10.2003
# DESCRIPTION:
# - I descrittori di files lay_descr.txt sono aperti solo in lettura e non anche in scrittura
# - Nel combo del project non vengono visualizzati i files, e le cartelle del report e trash
# - Pulsante per cancellare i moduli importati
# - Pulsante per settare il path di lavoro
# - Pulsante per cambiare gli stimoli
# DATE: 22.12.2003
# DESCRIPTION:
# - Viene esteso il Launcher anche all'NSI Tester e predisposto per lo SPIL. Per fare ciò viene preso in considerazione   e
#   anche il tag *INSTRUMENT presente nel lay_descr.txt. Le TD relative all'NSI saranno visualizzate in BLU
#   mentre rimarranno nere quelle del HIL. Quelle per lo SPIL sono in bianco.
#   Questo ha portato a modificare il file led_status.txt in quanto oltre alla lista dei LED vi è anche un flag  
#   che segnala la fine della sessione per un certo strumento.
# - E' stato aggiunto un pulsantino che permette di aprire il log. Rimane verde se non ci sono stati
#   problemi d'esecuzione, rosso altrimenti.
# - La dimensione X del radiobox è stata fissata anzichè  ottenerla automaticamente. In questo modo Test Case   
#   dal nome lungo non deformano la grafica.
# - Si fa un controllo sulla lunghezza del titolo della TD e se troppo lungo si diminuisce il font e si va a capo
# - Messa icona di APOLLO13 nella barra del frame e compilando con l'opzione python setup.py py2exe --icon launcher.ico
#   si ottiene icona per l'eseguibile. (solo se si compila da NT o Win2000)
# DATE: 22.12.2003
# DESCRIPTION:
# - E' cambiata la gestione del colore del pulsante di log. Per farlo diventare rosso si fa un confronto tra
#   lo stato dei led e i test selezionati. In questo modo il pulsante di log diventa rosso anche per i test di HIL
# DATE: 28.03.2004
# DESCRIPTION:
# - Correzione baco : in modalità debug lancio il primo TD selezionato e non il primo assoluto
# DATE: 06.04.2004
# DESCRIPTION:
# - Per i test NSI riconosco che c'è errore di compilazione se vedo diverso da 0 a sx di error(s) e non error 
#   in quanto vi possono essere casi di ambiguità
# - Se vi è un errore di sistema che  fa colorare il log di rosso scrivo nel log TEST FALLITO
#4.0.3 :
# Modifica gestione errori in caso di errata lettura del file load
# Correzione nome zip
# Modifica nome cartella report (messo i campi di gg hh mm ss tra parentesi)
#4.0.4 :
# Passo alla setvar anche il path del progetto
#4.0.5 :
# Gestisco OL e CL passandolo alla MatStop
# Creato pulsante nella finestra delle utility SPIL che converte gli output SPIL in output Matlab
#4.0.6 :
# Correzione Baco sulla selezione librerie di progetto
#4.0.7 :
# Settaggio variabile d'ambiente LIB x NSI in modalità RUN
# Aggiunto path del launcher nel file command
#4.0.8 :
# Possibilità di scegliere l'opzione coverage
# Lancio dello stimulus editor x SPIL
# Correzione baco su scelta progetto nel caso delle utility SPIL
# Creazione della cartella DD nel caso non esista
#4.0.9 :
# Nella modalità debug mancava un \n alla fine dell'IssStrtup.cmm 
#4.0.10 :
# Gestione degli errori nella fase di pre-processing post-processing e SPIL
#4.0.11 :
# Anche in modalità debug scrivo nel report la parte di SETUP
#4.0.12 :
#-Aggiunta chekbox nel dSPACE che cancella i file .pyc se nella stessa cartella vi sono file omonimi
#ma con estensione .py. La ricerca vieve effettuata nelle librerie, nei TD e nella cartella Work
#-Messo nella configurazione dell'HIL il checkbox per ottenere la lista del NS di riferimento
#-Aggiunta possibilità di ripetere la sequenza di test selezionati
#-Correzione baco nel caso non vi sia il file degli IR
#-Aggiunto pulsante SPIL x convertire pattern da STB a SDT
#4.0.13 :
#-Correzioni baco x l'iterzioni dei test
#4.0.14 :
#-Correzioni baco x il coverage quando non si sceglie il primo TC
#4.0.15:
#-Creazione del menù File in cui c'è l'opzione Exit
#-Correzione path assoluto nello SPIL per il caricamento del load.cmm
#4.0.16:
#-Messa opzione Debug IDLE x dspace
#4.0.17:
#-Gestione diagnostica errori nel MatStrtp solo in fase RUN
#4.0.18:
#-Correzione baco nella gestione della ripetizione dei test (in realtà non funzionava). Il for  è sulla RUN_TEST
#anzichè essere su ogni strumento. La fine del test si ha quando sono finiti tutti gli strumenti e anche il
#ciclo è finito. L'inizializzazione del file led_status.txt è fatto ad ogni ripetizione e quindi è stato sposta-   
#to nella RUN_TEST. Tra le altre modifiche la ripetizione non è abilitata in debug mode
#-Ci si svincola completamente dal percorso M:\work
#-Il browser dello stimulus editor dello SPIL diventa browser di file
#-Il change stimulus dell'HIL si colloca nella directory del progetto
#-Nel MatStrtp viene aggiunto il parametro CompareList nella IOCdiag
#-Correzione scrittura nel log di test eseguiti o falliti x NSI
#-
############################################################################################################


#################################################
#   INCLUDED FILES (libraries)           
#################################################
import sys
from wxPython.wx import*
import threading
import string
import glob
import os
import time
import traceback
import shutil
import stat
import time
import zipfile
import win32api
############################################################################################################
# FUNCTION NAME : GetSpilTdData
# FUNCTION DESCRIPTION:  
# Specificatamente per lo SPIL vengono creati automaticamente i campi di *MAIN,*TD e *TC prendendo le informazioni
# dai nomi presenti nel TD
#
# INPUT PARAMETERS:
#           1) TD: è il path della TD in cui si vanno a ricercare i files.
#
# OUTPUT PARAMETERS:
#           1) TD aggiornato con i campi *MAIN *TD e *TC
#           es:['M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM08_4TV', 'SpilMainDummy', ['SM08','SM08_TP_01_in', 'SM08_TP_02_in']]
############################################################################################################
def GetSpilTdData(TD):
    FileSearch=TD[0]+'\\*_TP_??_in.mat'
    TClist=glob.glob(FileSearch)
    tmp=[]
    TDname=[]
    for TCpath in TClist:
        tmp.append(os.path.splitext(os.path.basename(TCpath))[0])
    if tmp!=[]:
        tmp.sort()
        TDnamestp=string.find(tmp[-1],"_TP_")
        TDname=[tmp[-1][0:TDnamestp]]
        tmp=TDname+tmp
        TD[1]=('SpilMainDummy')
        TD.append(tmp)
    else:
        TD=[]
    return TD
        
        

############################################################################################################
# FUNCTION NAME : TdParse
# FUNCTION DESCRIPTION:  
# Vengono ricercati tutti i file che rispondono al path passato come argomento e analizzato il loro contenuto.
# Questi files devono contenere le caratteristiche della TD per il layout grafico e il nome del file main della TD
# Affinchè il TD sia trovato dal launcher, in ogni cartella TD deve esserci un file descrittore di TD es:lay_descr.txt 
#
# INPUT PARAMETERS:
#           1) file_path: è il path in cui si vanno a ricercare i files. (* è il simbolo jolly) es: "M:\Work\*\*\*\lay_descr.txt"
#
# OUTPUT PARAMETERS:
#           1) TD_DATA: è la lista dei dati raccolti dal parse in generale è:
#              TD_DATA=[['TD1path','TD1main',["TD1 NAME","TC1 NAME","TC2 NAME",..,"TCn NAME"]],...,['TDmpath','TDmmain',["TDm NAME","TC1 NAME",...,"TCz NAME"]]]
#              -TDxpath è il path della TD 
#              -TDxmain è il file main della TD 
#              -TDx NAME è la label del TD  
#              -TCx NAME è la label dei TC 
#           raccoglie i dati di tutti gli strumenti presenti nell'ordine HIL,NSI,SPIL
#           2) [0,RangeHIL,RangeNSI,RangeSPIL] poichè il TD_DATA non ha nessun riferimento a quali sono 
#               i test relativi ad uno strumento piuttosto che ad un'altro occorre passare questo vettore che
#               indica per ogni strumento quali TD fanno capo. Se per es ci fossero 3 TD x HIL,2 TD x NSI,0 TD x SPIL
#               il vettore sarebbe [0,3,5,5]
############################################################################################################

def TdParse(file_path):      
    #main_list è la lista dei file descrittori trovati con il criterio del file_path (ogni cartella TD deve averne uno solo)
    main_list=glob.glob(file_path)
    #LEGGO IL FILE .TXT DI OGNI TD
    TD_DATA=[]  #raccoglitore globale sul quale viene costruito il layout
    TD_DATA_HILdSPACE=[]    #raccoglitore dei test su dspace
    TD_DATA_NSI=[]  #raccoglitore test su NSI
    TD_DATA_SPIL=[] #raccoglitore test su SPIL
    for mainpy in main_list:
        #check flag che viene settato a 0 se il descrittore non è compilato correttamente per quanto riguarda le informazioni grafiche 
        check=1
        #path della TD
        path=os.path.dirname(mainpy)
        f=open(mainpy,'r')
        f.seek(0)
        testo=f.read()
        f.close()
        TD=[path]

        #Leggo il campo MAIN (file che viene lanciato dal launcher)        
        funzione_start=string.find(testo,"*MAIN=")
        if funzione_start>=0 :
            line_start=string.find(testo,"=",funzione_start)+1
            line_stop=string.find(testo,"\n",line_start)
            line=testo[line_start:line_stop]
            TD=TD+[line]
        else:
            check=0
    
        #Leggo il campo TD (label nome della TD)
        TC=[]
        td_start=string.find(testo,"*TD=")
        if td_start>=0 :
            line_start=string.find(testo,"=",td_start)+1
            line_stop=string.find(testo,"\n",line_start)
            line=testo[line_start:line_stop]        
            TC=TC+[line]
        else:
            check=0
    
        #Leggo il campo TC (label nome della TC)
        tc_start=0
        while tc_start>=0:
            tc_start=string.find(testo,"*TC=",tc_start+1)
            if tc_start>=0 :
                line_start=string.find(testo,"=",tc_start)+1
                line_stop=string.find(testo,"\n",line_start)
                line=testo[line_start:line_stop]
                TC=TC+[line]            
        if len(TC)<=1:
            check=0
        TD.append(TC)

        #Leggo il campo INSTRUMENT e inserisco i dati raccolti nella lista del relativo strumento
#        if check==1:
        funzione_start=string.find(testo,"*INSTRUMENT=")
        if funzione_start>=0 :
            line_start=string.find(testo,"=",funzione_start)+1
            line_stop=string.find(testo,"\n",line_start)
            line=testo[line_start:line_stop]
            if check==1 or line=="SPIL":
                if line=="HILdSPACE":
                    TD_DATA_HILdSPACE.append(TD)
                elif line=="NSI":
                    TD_DATA_NSI.append(TD)
                elif line=="SPIL":
                    TD=GetSpilTdData(TD)
                    if TD!=[]:
                        TD_DATA_SPIL.append(TD)
                else:
                    print "TD ",path,"non importata in quanto strumento non riconosciuto \n(correggere campo *INSTRUMENT nel lay_descr.txt)"
            else:
                print "TD ",path,"non importata in quanto non trovati i campi *MAIN e/o *TD e/o *TC nel lay_descr.txt"
        else:
            print "TD ",path,"non importata in quanto campo INSTRUMENT nel lay_descr.txt non trovato"

    #Metto in ordine i TD per strumento: prima HIL, poi NSI, poi SPIL
    if len(TD_DATA_HILdSPACE)!=0:
        TD_DATA=TD_DATA+TD_DATA_HILdSPACE
    if len(TD_DATA_NSI)!=0:
        TD_DATA=TD_DATA+TD_DATA_NSI
    if len(TD_DATA_SPIL)!=0:
        TD_DATA=TD_DATA+TD_DATA_SPIL
    #ritorno il TD_DATA e anche una lista nella quale sono indicati i range dei TD strumento x strumento
    RangeHIL=len(TD_DATA_HILdSPACE)
    RangeNSI=RangeHIL+len(TD_DATA_NSI)
    RangeSPIL=RangeNSI+len(TD_DATA_SPIL)
    return TD_DATA,[0,RangeHIL,RangeNSI,RangeSPIL]



############################################################################################################
# FUNCTION NAME : SelectorFrame
# FUNCTION DESCRIPTION: Creazione della finestra contenente i TD, i TC e i led
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def SelectorFrame(self):

    #self.scmain.sc --> finestra con lo scroll
    self.scmain.sc=wxScrolledWindow(self.scmain, -1,pos = (200,0), size = (self.ScreenWidth*1.3-200,self.ScreenHeight*1.3-20), style = wxHSCROLL | wxVSCROLL, name = "scrolledWindow")
    self.scmain.sc.SetScrollbars(10, 10, 1000, 1000, xPos = 0, yPos = 0, noRefresh = FALSE)

    #Caratteristiche grafiche
    TDW=200     #larghezza TD*
    #TCH=40      #altezza TC
    LEDW=20     #larghezza led*
    LEDH=20     #altezza led*
    DXLED=165   #delta x led*
    DYLED=15    #delta y led*
    #DXTD=197    #delta x TD*
    DXTD=200    #delta x TD*
    DYTC=50     #delta y TC*
    DXTC=5     #delta x TC*
    SXTD=20     #start x TD*
    SYTD=10     #start y TD*
    TCALLW=50  #larghezza TCALL
    TCALLH=20   #altezza TCALL
    DYTCALL=60 #delta y TCALL 
    TCdefvalue=2 #posizione di default del radio
    DYWIN=85    #spazio al di sotto dei TCALL
    LEDdefvalue=0 #posizione di default del radio
    sampleList = ['NO   ', 'OL   ', 'CL   ']
    font = wxFont(18, wxSWISS, wxNORMAL, wxNORMAL, False, "Arial")


    #creo TD
    posx=SXTD
    posy=SYTD
    for indexTD in range(len(self.TD_DATA)):
        posy=SYTD
        #self.scmain.sc.TDx --> finestra in cui x è il numero della TD considerataa
        TDH=(len(self.TD_DATA[indexTD][2])-1)*DYTC+TCALLH+DYWIN
        str2exe="self.scmain.sc.TD"+str(indexTD+1)+" = wxWindow(self.scmain.sc, -1, (posx,posy), (TDW,TDH), wxSUNKEN_BORDER)"
        #es: self.scmain.sc.TD1 = wxWindow(self.scmain.sc, -1, (posx,posy), (TDW,TDH), wxSUNKEN_BORDER)
        exec(str2exe)
        #Se la lunghezza del nome della TD è> 15 allora rimpicciollisci e va a capo 
        if len(self.TD_DATA[indexTD][2][0])>14:
            self.TD_DATA[indexTD][2][0]=self.TD_DATA[indexTD][2][0][:17]+"\\n"+self.TD_DATA[indexTD][2][0][17:]
            font = wxFont(14, wxSWISS, wxNORMAL, wxNORMAL, False, "Arial")
        str2exe="self.scmain.sc.TD"+str(indexTD+1)+".textTD"+str(indexTD+1)+"=wxStaticText(self.scmain.sc.TD"+str(indexTD+1)+", -1, '"+self.TD_DATA[indexTD][2][0]+"', wxDefaultPosition, wxDefaultSize)"
        #es: self.scmain.sc.TD1.textTD1 = =wxStaticText(self.scmain.sc.TD1, -1, "TD GLAM", wxDefaultPosition, wxDefaultSize)
        exec(str2exe)
        str2exe="self.scmain.sc.TD"+str(indexTD+1)+".textTD"+str(indexTD+1)+".SetFont(font)"
        #es: self.scmain.sc.TD1.textTD1.SetFont(font)
        exec(str2exe)
        #In base al tipo di strumento cambio il colore nella visualizzazione
        #colore BLU per NSI
        if self.TD_NumTdXInstr[1]<=indexTD<self.TD_NumTdXInstr[2]:
            str2exe="self.scmain.sc.TD"+str(indexTD+1)+".textTD"+str(indexTD+1)+".SetForegroundColour(wxBLUE)"
            exec(str2exe)
        #colore BIANCO per SPIL
        if self.TD_NumTdXInstr[2]<=indexTD<self.TD_NumTdXInstr[3]:
            str2exe="self.scmain.sc.TD"+str(indexTD+1)+".textTD"+str(indexTD+1)+".SetForegroundColour(wxWHITE)"
            exec(str2exe)


        #creo radio TC e LED        
        for indexTC in range(len(self.TD_DATA[indexTD][2])-1):
            posy=posy+DYTC
            #self.scmain.sc.TDx.rby_x --> radiobutton dove x è il numero della TD y del TC considerato,
            str2exe="self.scmain.sc.TD"+str(indexTD+1)+".rb"+str(indexTC+1)+"_"+str(indexTD+1)+" = wxRadioBox(self.scmain.sc.TD"+str(indexTD+1)+", -1, '"+self.TD_DATA[indexTD][2][indexTC+1]+"', wxDefaultPosition, (155,-1),sampleList)"
            #es: self.scmain.sc.TD1.rb1_1 = wxRadioBox(self.scmain.sc.TD1, -1, "tc name", wxDefaultPosition, wxDefaultSize,sampleList)
            exec(str2exe)
            str2exe="self.scmain.sc.TD"+str(indexTD+1)+".rb"+str(indexTC+1)+"_"+str(indexTD+1)+".SetPosition((DXTC,posy))"
            #es: self.scmain.sc.TD1.rb1_1.SetPosition((DXTC,posy))
            exec(str2exe)
            str2exe="self.scmain.sc.TD"+str(indexTD+1)+".rb"+str(indexTC+1)+"_"+str(indexTD+1)+".SetSelection(2)"
            #es: self.scmain.sc.TD1.rb1_1.SetSelection(2)
            exec(str2exe)
            #self.scmain.sc.TDx.ledy_x --> led dove x è il numero della TD y del TC considerato,
            str2exe="self.scmain.sc.TD"+str(indexTD+1)+".led"+str(indexTC+1)+"_"+str(indexTD+1)+" = wxWindow(self.scmain.sc.TD"+str(indexTD+1)+", -1, (DXLED,posy+DYLED), (LEDW,LEDH), wxSUNKEN_BORDER)"
            #es: self.scmain.sc.TD1.led1_1 = wxWindow(self.scmain.sc.TD1, -1, (DXLED,posy+DYLED), (LEDW,LEDH), wxSUNKEN_BORDER)
            exec(str2exe)
            #colore BLU per NSI
            if self.TD_NumTdXInstr[1]<=indexTD<self.TD_NumTdXInstr[2]:
                str2exe="self.scmain.sc.TD"+str(indexTD+1)+".rb"+str(indexTC+1)+"_"+str(indexTD+1)+".SetForegroundColour(wxBLUE)"
                exec(str2exe)
            #colore BIANCO per SPIL
            if self.TD_NumTdXInstr[2]<=indexTD<self.TD_NumTdXInstr[3]:
                str2exe="self.scmain.sc.TD"+str(indexTD+1)+".rb"+str(indexTC+1)+"_"+str(indexTD+1)+".SetForegroundColour(wxWHITE)"
                exec(str2exe)


        #nox=xy --> dove x TD considerata e y è la funzione associata:0=NO, 1=OL, 2=CL 
        str2exe="no"+str(indexTD+1)+"="+str(indexTD+1)+"0"
        #es: no1=10
        exec(str2exe)
        str2exe="ol"+str(indexTD+1)+"="+str(indexTD+1)+"1"
        #es: ol1=11
        exec(str2exe)
        str2exe="cl"+str(indexTD+1)+"="+str(indexTD+1)+"2"
        #es: cl1=12
        exec(str2exe)

        #creo pulsanti e relativi eventi per la selezione rapida nel TD
        #NO
        str2exe="self.scmain.sc.TD"+str(indexTD+1)+".no"+str(indexTD+1)+"=wxButton(self.scmain.sc.TD"+str(indexTD+1)+", no"+str(indexTD+1)+",'NO',(DXTC,posy+DYTCALL),size=(TCALLW,TCALLH))"
        #es: self.scmain.sc.TD1.no1=wxButton(self.scmain.sc.TD1, no1,'NO',(DXTC,posy+DYTCALL),size=(TCALLW,TCALLH))
        exec(str2exe)
        str2exe="EVT_BUTTON(self, no"+str(indexTD+1)+", self.EvtTcAll)"
        #es: EVT_BUTTON(self, no1, self.EvtTcAll)
        exec(str2exe)
        #OL
        str2exe="self.scmain.sc.TD"+str(indexTD+1)+".ol"+str(indexTD+1)+"=wxButton(self.scmain.sc.TD"+str(indexTD+1)+", ol"+str(indexTD+1)+",'OL',(DXTC+TCALLW,posy+DYTCALL),size=(TCALLW,TCALLH))"
        #es: self.scmain.sc.TD1.ol1 = wxButton(self.scmain.sc.TD1, ol1,'OL',(DXTC+TCALLW,posy+DYTCALL),size=(TCALLW,TCALLH))
        exec(str2exe)
        str2exe="EVT_BUTTON(self, ol"+str(indexTD+1)+", self.EvtTcAll)"
        #es: EVT_BUTTON(self, ol1, self.EvtTcAll)
        exec(str2exe)
        #CL
        str2exe="self.scmain.sc.TD"+str(indexTD+1)+".cl"+str(indexTD+1)+"=wxButton(self.scmain.sc.TD"+str(indexTD+1)+", cl"+str(indexTD+1)+",'CL',(DXTC+2*TCALLW,posy+DYTCALL),size=(TCALLW,TCALLH))"
        #es: self.scmain.sc.TD1.cl1 = wxButton(self.scmain.sc.TD1, cl1,'CL',(DXTC+2*TCALLW,posy+DYTCALL),size=(TCALLW,TCALLH))
        exec(str2exe)
        str2exe="EVT_BUTTON(self, cl"+str(indexTD+1)+", self.EvtTcAll)"
        #es: EVT_BUTTON(self, cl1, self.EvtTcAll)
        exec(str2exe)

        posx=posx+DXTD



############################################################################################################
# FUNCTION NAME : TdBrowse
# FUNCTION DESCRIPTION: Creazione di un box contenente la lista dei TD scaricati
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
#
############################################################################################################
def TdBrowse(self):
    #compongo la lista prendendo le label dei descrittori di TD
    self.TDlist=[]
    for indexTD in self.TD_DATA:
        self.TDlist.append(indexTD[2][0])
    self.wnd.launcher.ch = wxListBox(self.wnd.launcher, 5, (5, 220),(150,100), choices = self.TDlist)
    #setto evento della lista
    EVT_LISTBOX(self, 5, self.EvtChoice)



############################################################################################################
# FUNCTION NAME : LauncherFrame
# FUNCTION DESCRIPTION: Creazione di una finestra contenente:pulsante di GO, selezione globale dei TC, TdBrowse, selezione tipo ecu 
#                       Creazione pulsante GO
#                       Creazione pulsanti NO OL CL
#                       Creazione radiobox tipo di ecu
#                       Creazione radiobox per modalità di lancio 
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def LauncherFrame(self):
    #Creazione finestra launcher
    self.wnd.launcher = wxWindow(self.wnd, -1, (10,10), (170,400), wxSUNKEN_BORDER)
    self.wnd.lbl=wxStaticText(self.wnd.launcher, -1,"  LAUNCHER", wxDefaultPosition, wxDefaultSize)
    font = wxFont(18, wxSWISS, wxNORMAL, wxNORMAL, False, "Arial")
    self.wnd.lbl.SetFont(font)
    #Creazione radiobox per selezione tipo ecu
    self.wnd.launcher.ECU = wxRadioBox(self.wnd.launcher, -1,"", (5,30), wxDefaultSize,["Ecu Svil.","Ecu Prod."])
    #creazione pulsante GO
    self.wnd.GO=wxButton(self.wnd.launcher, 3,'GO',pos=(5,80),size=(150,70))
    EVT_BUTTON(self,3,self.EvtGO)
    #Creazione SpinButton x ripetizione test
    self.wnd.RptTestTxt = wxTextCtrl(self.wnd.launcher, 148, "1", wxPoint(65, 190), wxSize(30, 23))
    self.wnd.RptTestSpn = wxSpinButton(self.wnd.launcher, 149, wxPoint(98, 190), wxSize(20, 22), wxSP_VERTICAL)
    self.wnd.RptTestLbl = wxStaticText(self.wnd.launcher, -1, "Repeat for", pos=(5,195), size=(55,20),style=wxALIGN_LEFT)
    self.wnd.RptTestLbl = wxStaticText(self.wnd.launcher, -1, "times", pos=(122,195), size=(25,20),style=wxALIGN_RIGHT)
    self.wnd.RptTestSpn.SetRange(1, 100)
    self.wnd.RptTestSpn.SetValue(1)
    EVT_SPIN(self.wnd.RptTestSpn, 149, self.RptTest)
    #creazione pulsante NO
    self.wnd.launcher.NO=wxButton(self.wnd.launcher, 0,'NO',pos=(5,150),size=(50,30))
    EVT_BUTTON(self,0,self.EvtTcAll)
    #creazione pulsante OL
    self.wnd.launcher.OL=wxButton(self.wnd.launcher, 1,'OL',pos=(55,150),size=(50,30))
    EVT_BUTTON(self,1,self.EvtTcAll)
    #creazione pulsante CL
    self.wnd.launcher.CL=wxButton(self.wnd.launcher, 2,'CL',pos=(105,150),size=(50,30))
    EVT_BUTTON(self,2,self.EvtTcAll)
    #Creazione selettore per modalità di lancio  run o debug 
    self.wnd.launcher.run = wxRadioBox(self.wnd.launcher, -1,"Launch Modality", (5,325), wxDefaultSize,["Run       ","Debug     "])
    #Creazione del pulsanti di log
    self.wnd.launcher.log=wxButton(self.wnd.launcher, 116,'Log',pos=(5,375),size=(25,15))
    EVT_BUTTON(self,116,self.EvtLog)
    

############################################################################################################
# FUNCTION NAME : ReloadButton
# FUNCTION DESCRIPTION: Creazione pulsante per il rinfresco della finestra del TD Selector
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def ReloadButton(self):
    #RELOAD
    self.wnd.reload=wxButton(self.wnd, 6,'RELOAD',pos=(5,470),size=(180,40))
    EVT_BUTTON(self,6,self.EvtReload)

############################################################################################################
# FUNCTION NAME : MenuBar
# FUNCTION DESCRIPTION: Creazione Menu per la configurazione dell'ambiente di lavoro
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def MenuBar(self):
    menuBar = wxMenuBar()
    # menu0 = File menu per Exit
    menu0 = wxMenu()
    menu0.Append(156, "&Exit      Alt+F4", "")
    # menu1 = General Set per settare il path dei progetti e dei files descrittori di TD
    menu1 = wxMenu()
    menu1.Append(108, "&Td Set...", "")
    # menu2 = Instruments Set per settare i path dello strumento
    menu2 = wxMenu()
    menu2.Append(105, "&dSPACE HIL...", "")
    menu2.Append(106, "&NSI...", "")
    menu2.Append(107, "&SPIL...", "")
    # Add menu to the menu bar
    menuBar.Append(menu0, "&File")
    menuBar.Append(menu1, "&General Set")
    menuBar.Append(menu2, "&Instruments Set")
    self.SetMenuBar(menuBar)
    #setto eventi
    EVT_MENU(self, 105, self.Configuration)
    EVT_MENU(self, 108, self.Configuration)
    EVT_MENU(self, 106, self.Configuration)
    EVT_MENU(self, 107, self.Configuration)
    EVT_MENU(self, 156, self.Configuration)
############################################################################################################
# FUNCTION NAME : PyConfig
# FUNCTION DESCRIPTION: Creazione delle finestre di settaggio configurazione del python
#                        -Python.exe
#                        -Pythonwin.exe
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def PyConfig(self):
    #creazione frame generale
    self.PyConfigFrame=wxMiniFrame(None, -1, title="Hil Configuration", pos=(200,100), size=(500,300), style=wxDEFAULT_FRAME_STYLE)
    self.PyConfigFrame.Panel = wxPanel(self.PyConfigFrame, -1,pos=(0,0), size=(500,300))
    #creazione text control e browse di Python.exe
    self.PyConfigFrame.Panel.PythonLabel = wxStaticText(self.PyConfigFrame.Panel, -1, "Python.exe:", pos=(5,7), size=(95,20),style=wxALIGN_RIGHT)
    self.PyConfigFrame.Panel.PythonText = wxTextCtrl(self.PyConfigFrame.Panel, -1, self.PythonPath,pos=(100,5), size=(300, 20))
    self.PyConfigFrame.Panel.PythonBrowse =wxButton(self.PyConfigFrame.Panel, 1, "...",pos=(405,5),size=(18,20))
    EVT_BUTTON(self.PyConfigFrame.Panel.PythonBrowse, 1, self.OnBrowse)
    #creazione text control e browse di Pythonwin.exe
    self.PyConfigFrame.Panel.PythonwinLabel = wxStaticText(self.PyConfigFrame.Panel, -1, "Pythonwin.exe:\nidle.pyw:", pos=(5,40), size=(95,50),style=wxALIGN_CENTRE)
    self.PyConfigFrame.Panel.PythonwinText = wxTextCtrl(self.PyConfigFrame.Panel, -1, self.PythonwinPath,pos=(100,45), size=(300, 20))
    self.PyConfigFrame.Panel.PythonwinBrowse =wxButton(self.PyConfigFrame.Panel, 2, "...",pos=(405,45),size=(18,20))
    EVT_BUTTON(self.PyConfigFrame.Panel.PythonwinBrowse, 2, self.OnBrowse)
    #creazione pulsante per creazione del ClearNS
    self.PyConfigFrame.Panel.CreateNSBox = wxCheckBox(self.PyConfigFrame.Panel, 147, "Generate Pythonwin NS reference (PythonWin must be down)", wxPoint(100, 80), wxSize(380, 30), wxNO_BORDER)
    self.PyConfigFrame.Panel.CreateNSBox.SetValue(False)
    #self.wnd.Develop.AddPage(self.wnd.Develop.Python, "dSPACE")

    
    #self.PyConfigFrame.Panel.CreateNS=wxButton(self.PyConfigFrame.Panel, 147,'Create NS',pos=(150,100),size=(200,40))
    #EVT_BUTTON(self.PyConfigFrame.Panel.CreateNS,147,self.CreateNS)
    #creazione pulsante salvataggio
    self.PyConfigFrame.Panel.SavePyConfig=wxButton(self.PyConfigFrame.Panel, 4,'Save Config',pos=(150,200),size=(200,40))
    EVT_BUTTON(self.PyConfigFrame.Panel.SavePyConfig,4,self.SaveConfig)
    self.PyConfigFrame.Show(1)



############################################################################################################
# FUNCTION NAME : GenConfig
# FUNCTION DESCRIPTION: Creazione delle finestre di settaggio configurazione generale del launcher
#                       - lay_descr.txt
#                       - Project Dir
#                        -PythonLib
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def GenConfig(self):
    #creazione frame generale
    self.GenConfigFrame=wxMiniFrame(None, -1, title="General Configuration", pos=(200,100), size=(500,300), style=wxDEFAULT_FRAME_STYLE)
    self.GenConfigFrame.Panel = wxPanel(self.GenConfigFrame, -1,pos=(0,0), size=(500,300))
    #creazione text control e browse di lay_descr.txt
    self.GenConfigFrame.Panel.DescrLabel = wxStaticText(self.GenConfigFrame.Panel, -1, "lay_descr.txt:", pos=(5,87), size=(95,20),style=wxALIGN_RIGHT)
    self.GenConfigFrame.Panel.DescrText = wxTextCtrl(self.GenConfigFrame.Panel, -1, self.DescrPath,pos=(100,85), size=(300, 20))
    self.GenConfigFrame.Panel.DescrBrowse =wxButton(self.GenConfigFrame.Panel, 5, "...",pos=(405,85),size=(18,20))
    EVT_BUTTON(self.GenConfigFrame.Panel.DescrBrowse, 5, self.OnBrowse)
    #creazione text control e browse di Project Dir
    self.GenConfigFrame.Panel.ProjLabel = wxStaticText(self.GenConfigFrame.Panel, -1, "Project Dir:", pos=(5,47), size=(95,20),style=wxALIGN_RIGHT)
    self.GenConfigFrame.Panel.ProjText = wxTextCtrl(self.GenConfigFrame.Panel, -1, self.ProjPath,pos=(100,45), size=(300, 20))
    self.GenConfigFrame.Panel.ProjBrowse =wxButton(self.GenConfigFrame.Panel, 7, "...",pos=(405,45),size=(18,20))
    EVT_BUTTON(self.GenConfigFrame.Panel.ProjBrowse, 7, self.OnBrowse)
    #creazione text control e browse di PythonLib
    self.GenConfigFrame.Panel.PylibLabel = wxStaticText(self.GenConfigFrame.Panel, -1, "LibPath:", pos=(5,127), size=(95,20),style=wxALIGN_RIGHT)
    self.GenConfigFrame.Panel.PylibText = wxTextCtrl(self.GenConfigFrame.Panel, -1, self.PylibPath,pos=(100,125), size=(300, 20))
    self.GenConfigFrame.Panel.PylibBrowse =wxButton(self.GenConfigFrame.Panel, 3, "...",pos=(405,125),size=(18,20))
    EVT_BUTTON(self.GenConfigFrame.Panel.PylibBrowse, 3, self.OnBrowse)
    #creazione pulsante salvataggio
    self.GenConfigFrame.Panel.SaveGenConfig=wxButton(self.GenConfigFrame.Panel, 6,'Save Config',pos=(150,200),size=(200,40))
    EVT_BUTTON(self.GenConfigFrame.Panel.SaveGenConfig,6,self.SaveConfig)
    self.GenConfigFrame.Show(1)

############################################################################################################
# FUNCTION NAME : NSIConfig
# FUNCTION DESCRIPTION: Creazione delle finestre di settaggio configurazione dello strumento NSI
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def NSIConfig(self):
    #creazione frame generale
    self.NSIConfigFrame=wxMiniFrame(None, -1, title="NSI Configuration", pos=(200,100), size=(500,300), style=wxDEFAULT_FRAME_STYLE)
    self.NSIConfigFrame.Panel = wxPanel(self.NSIConfigFrame, -1,pos=(0,0), size=(500,300))
    #creazione text control e browse di MSDevDir
    self.NSIConfigFrame.Panel.MSDevDirLabel = wxStaticText(self.NSIConfigFrame.Panel, -1, "MSDevDir:", pos=(5,37), size=(95,20),style=wxALIGN_RIGHT)
    self.NSIConfigFrame.Panel.MSDevDirText = wxTextCtrl(self.NSIConfigFrame.Panel, -1, self.NSIMSDevDir,pos=(100,35), size=(300, 20))
    self.NSIConfigFrame.Panel.MSDevDirBrowse =wxButton(self.NSIConfigFrame.Panel, 117, "...",pos=(405,35),size=(18,20))
    EVT_BUTTON(self.NSIConfigFrame.Panel.MSDevDirBrowse, 117, self.OnBrowse)
    #creazione text control e browse di MSVCDir
    self.NSIConfigFrame.Panel.MSVCDirLabel = wxStaticText(self.NSIConfigFrame.Panel, -1, "MSVCDir:", pos=(5,87), size=(95,20),style=wxALIGN_RIGHT)
    self.NSIConfigFrame.Panel.MSVCDirText = wxTextCtrl(self.NSIConfigFrame.Panel, -1, self.NSIMSVCDir,pos=(100,85), size=(300, 20))
    self.NSIConfigFrame.Panel.MSVCDirBrowse =wxButton(self.NSIConfigFrame.Panel, 118, "...",pos=(405,85),size=(18,20))
    EVT_BUTTON(self.NSIConfigFrame.Panel.MSVCDirBrowse, 118, self.OnBrowse)
    #creazione text control e browse di NSIDir
    self.NSIConfigFrame.Panel.NSIDirLabel = wxStaticText(self.NSIConfigFrame.Panel, -1, "NSIDir:", pos=(5,137), size=(95,20),style=wxALIGN_RIGHT)
    self.NSIConfigFrame.Panel.NSIDirText = wxTextCtrl(self.NSIConfigFrame.Panel, -1, self.NSIDir,pos=(100,135), size=(300, 20))
    self.NSIConfigFrame.Panel.NSIDirBrowse =wxButton(self.NSIConfigFrame.Panel, 123, "...",pos=(405,135),size=(18,20))
    EVT_BUTTON(self.NSIConfigFrame.Panel.NSIDirBrowse, 123, self.OnBrowse)
    #creazione pulsante salvataggio
    self.NSIConfigFrame.Panel.SaveNSIConfigFrame=wxButton(self.NSIConfigFrame.Panel, 119,'Save Config',pos=(150,200),size=(200,40))
    EVT_BUTTON(self.NSIConfigFrame.Panel.SaveNSIConfigFrame,119,self.SaveConfig)
    self.NSIConfigFrame.Show(1)


############################################################################################################
# FUNCTION NAME : SPILConfig
# FUNCTION DESCRIPTION: Creazione delle finestre di settaggio configurazione dello strumento SPIL
#                       MatLabInstallation=r'C:\Programmi\matlab65p1\bin\win32\matlab.exe'  #da configurazione
#                       StartUpMatlabPath=r'C:\Programmi\matlab65p1\toolbox\local\startup'  #da configurazione
#                       IssInstallation=r'C:\Programmi\T32\T32M166.EXE'  #da configurazione
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def SPILConfig(self):
    #creazione frame generale
    self.SPILConfigFrame=wxMiniFrame(None, -1, title="SPIL Configuration", pos=(200,100), size=(500,300), style=wxDEFAULT_FRAME_STYLE)
    self.SPILConfigFrame.Panel = wxPanel(self.SPILConfigFrame, -1,pos=(0,0), size=(500,300))
    #creazione text control e browse di MatlabInstallation
    self.SPILConfigFrame.Panel.MtlbInstallLabel = wxStaticText(self.SPILConfigFrame.Panel, -1, "Matlab.exe:", pos=(5,37), size=(95,20),style=wxALIGN_RIGHT)
    self.SPILConfigFrame.Panel.MtlbInstallText = wxTextCtrl(self.SPILConfigFrame.Panel, -1, self.MtlbInstall,pos=(100,35), size=(300, 20))
    self.SPILConfigFrame.Panel.MtlbInstallBrowse =wxButton(self.SPILConfigFrame.Panel, 124, "...",pos=(405,35),size=(18,20))
    EVT_BUTTON(self.SPILConfigFrame.Panel.MtlbInstallBrowse, 124, self.OnBrowse)
    #creazione text control e browse di StartUpMatlabPath
    self.SPILConfigFrame.Panel.StimulusLabel = wxStaticText(self.SPILConfigFrame.Panel, -1, "waveman.exe:", pos=(5,87), size=(95,20),style=wxALIGN_RIGHT)
    self.SPILConfigFrame.Panel.StimulusText = wxTextCtrl(self.SPILConfigFrame.Panel, -1, self.Stimulus,pos=(100,85), size=(300, 20))
    self.SPILConfigFrame.Panel.StimulusBrowse =wxButton(self.SPILConfigFrame.Panel, 125, "...",pos=(405,85),size=(18,20))
    EVT_BUTTON(self.SPILConfigFrame.Panel.StimulusBrowse, 125, self.OnBrowse)
    #creazione text control e browse di IssInstallation
    self.SPILConfigFrame.Panel.IssInstallLabel = wxStaticText(self.SPILConfigFrame.Panel, -1, "T32xxx.exe:", pos=(5,137), size=(95,20),style=wxALIGN_RIGHT)
    self.SPILConfigFrame.Panel.IssInstallText = wxTextCtrl(self.SPILConfigFrame.Panel, -1, self.IssInstall,pos=(100,135), size=(300, 20))
    self.SPILConfigFrame.Panel.IssInstallBrowse =wxButton(self.SPILConfigFrame.Panel, 126, "...",pos=(405,135),size=(18,20))
    EVT_BUTTON(self.SPILConfigFrame.Panel.IssInstallBrowse, 126, self.OnBrowse)
    #creazione text control e browse di ReportSpil
    self.SPILConfigFrame.Panel.ReportSpilLabel = wxStaticText(self.SPILConfigFrame.Panel, -1, "ReportDir:", pos=(5,187), size=(95,20),style=wxALIGN_RIGHT)
    self.SPILConfigFrame.Panel.ReportSpilText = wxTextCtrl(self.SPILConfigFrame.Panel, -1, self.ReportSpil,pos=(100,185), size=(300, 20))
    self.SPILConfigFrame.Panel.ReportSpilBrowse =wxButton(self.SPILConfigFrame.Panel, 128, "...",pos=(405,185),size=(18,20))
    EVT_BUTTON(self.SPILConfigFrame.Panel.ReportSpilBrowse, 128, self.OnBrowse)

    #creazione pulsante salvataggio
    self.SPILConfigFrame.Panel.SaveSPILConfigFrame=wxButton(self.SPILConfigFrame.Panel, 127,'Save Config',pos=(150,230),size=(200,40))
    EVT_BUTTON(self.SPILConfigFrame.Panel.SaveSPILConfigFrame,127,self.SaveConfig)
    self.SPILConfigFrame.Show(1)


############################################################################################################
# FUNCTION NAME : ExitProc
# FUNCTION DESCRIPTION: Processo di chiusura del Launcher
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def ExitProc(self):
    self.Destroy()



############################################################################################################
# FUNCTION NAME : ProjectCombo
# FUNCTION DESCRIPTION: Creazione di un combobox contenente i progetti scaricati. Nel Selector Frame vi saranno
#                       solo i TD relativi al progetto selezionato
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def ProjectCombo(self):
    #Escludo dal combo i files e le cartelle indicate in Dir2NoShow
    self.Prj_Lst=[]
    FileList=glob.glob(self.ProjPath)
    Dir2NoShow=[string.lower(os.path.split(self.ProjPath)[0]+'\\reports'),string.lower(os.path.split(self.ProjPath)[0]+'\\trash')]
    for File in FileList:
        if os.path.isdir(File) and not(Dir2NoShow.count(os.path.normcase(File))):
            self.Prj_Lst.append(File)
    if self.Prj_Lst.count(self.CurrentPrj):
        InitProj=self.CurrentPrj
    else:
        if len(self.Prj_Lst)==0:
            InitProj=''
        else:
            InitProj=self.Prj_Lst[0]
    wxStaticText(self.wnd, -1, "Project:", wxPoint(5, 420), wxSize(75, 18))
    self.wnd.Project = wxComboBox(self.wnd, 500, InitProj, wxPoint(5, 440), wxSize(180, -1),self.Prj_Lst, wxCB_DROPDOWN)#|wxTE_PROCESS_ENTER)


############################################################################################################
# FUNCTION NAME : DevelopUtility
# FUNCTION DESCRIPTION: Creazione di un NoteBook per gestire utility dell'ambiente di sviluppo.
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def DevelopUtility(self):
    self.wnd.Develop=wxNotebook(self.wnd, -1,pos=(5,525),size=(180,150))

    #Tendina del dSPACE
    self.wnd.Develop.Python = wxWindow(self.wnd.Develop, -1, (0,0), (30,30), wxSUNKEN_BORDER      )
    self.wnd.Develop.Python.Path=wxButton(self.wnd.Develop.Python, 113,'Set Path',(5,5),size=(155,22))
    EVT_BUTTON(self,113,self.EvtPyUtility)
    self.wnd.Develop.Python.Clear=wxButton(self.wnd.Develop.Python, 114,'Clr Module',(5,30),size=(155,22))
    EVT_BUTTON(self,114,self.EvtPyUtility)
    self.wnd.Develop.Python.ChangeStim=wxButton(self.wnd.Develop.Python, 115,'Change Stim',(5,55),size=(155,22))
    EVT_BUTTON(self,115,self.EvtPyUtility)
    #checkbox per la cancellazione dei pyc
    self.wnd.Develop.Python.PycBox = wxCheckBox(self.wnd.Develop.Python, 146, "  Clear .pyc", wxPoint(5, 80), wxSize(150, 20), wxNO_BORDER)
    self.wnd.Develop.Python.PycBox.SetValue(False)
    self.wnd.Develop.AddPage(self.wnd.Develop.Python, "dSPACE")

    #Tendina dell' NSI
    self.wnd.Develop.Nsi = wxWindow(self.wnd.Develop, -1, (0,0), (30,30), wxSUNKEN_BORDER      )
    self.wnd.Develop.AddPage(self.wnd.Develop.Nsi, "NSI")

    #Tendina dello SPIL
    self.wnd.Develop.Spil = wxWindow(self.wnd.Develop, -1, (0,0), (30,30), wxSUNKEN_BORDER      )
    #time x il debug    
    self.wnd.Develop.Spil.DebugTimeLabel=wxStaticText(self.wnd.Develop.Spil, -1, "Debug Stop Time(ms):",pos=(5,30), size=(95,30),style=wxALIGN_LEFT)
    self.wnd.Develop.Spil.DebugTimeText = wxTextCtrl(self.wnd.Develop.Spil, -1, "0",pos=(80,33), size=(50, 20))
    #pulsante x creazione sdt
    self.wnd.Develop.Spil.SdtOut=wxButton(self.wnd.Develop.Spil, 129,'Output --> Sdt',(5,65),size=(75,20))
    EVT_BUTTON(self,129,self.EvtSpilUtility)
    #checkbox del coverage
    self.wnd.Develop.Spil.CovBox = wxCheckBox(self.wnd.Develop.Spil, 138, "  Coverage Enable", wxPoint(5, 5), wxSize(150, 20), wxNO_BORDER)
    self.wnd.Develop.Spil.CovBox.SetValue(False)
    #pulsante x apertura stimulus editor
    self.wnd.Develop.Spil.OpenStim=wxButton(self.wnd.Develop.Spil, 139,'STIM',(90,65),size=(35,20))
    EVT_BUTTON(self,139,self.EvtSpilUtility)
    #pulsante x conversione da stimolo a sdt
    self.wnd.Develop.Spil.OpenStim=wxButton(self.wnd.Develop.Spil, 143,'GEN',(125,65),size=(35,20))
    EVT_BUTTON(self,143,self.EvtSpilUtility)
    #pulsante x conversione da STB a sdt
    self.wnd.Develop.Spil.STB=wxButton(self.wnd.Develop.Spil, 153,'Stb --> Sdt',(5,95),size=(75,20))
    EVT_BUTTON(self,153,self.EvtSpilUtility)

    self.wnd.Develop.AddPage(self.wnd.Develop.Spil, "SPIL")







############################################################################################################
# CLASS NAME:   TdSelectorLay 
# CLASS DESCRIPTION: Gestione della grafica:
#                        -Creazione frame di base
#                        -Inizializzazione grafica
#                        -Gestione eventi 
#
# PARAMETERS:       wxFrame: oggetto frame
#
# LIST OF METHODS:   -OnBrowse
#                    -SaveConfig
#                    -EvtChoice
#                    -EvtReload
#                    -EvtTcAll
#                    -EvtGO
#                    -Configuration
############################################################################################################
class TdSelectorLay(wxFrame):

    def __init__(self):
        self.LauncherPath=os.getcwd()   #lavoro sempre sulla cartella in cui è presente questo eseguibile
        self.PythonPath="c:\\Programmi\\dSPACE\\dspace_r34_mlcu\\ControlDesk\\Python\\Python.exe"
        self.PythonwinPath="c:\\Programmi\\dSPACE\\dspace_r34_mlcu\\ControlDesk\\Python\\Pythonwin\\Pythonwin.exe"
        self.PylibPath="M:\\Work\\*\\Lib\\*"
        self.DescrPath="M:\\Work\\*\\*\\*\\lay_descr.txt"
        self.ProjPath="M:\\Work\\*"
        self.NSIMSDevDir=r"C:\Programmi\Microsoft Visual Studio\Common\MSDev98"
        self.NSIMSVCDir=r"C:\Programmi\Microsoft Visual Studio\VC98"
        self.NSIDir=r"C:\MMB"
        self.MtlbInstall=r"C:\Programmi\matlab65p1\bin\win32\matlab.exe"
        self.Stimulus=r"C:\Programmi\waveman\waveman.exe"
        self.IssInstall=r"C:\Programmi\T32\T32M166.EXE"
        self.ReportSpil=r"M:\Work\Reports"



        #se esiste il file config.txt in cui ho già configurato il launcher carico quello altrimenti prendo i default 
        if os.path.exists(self.LauncherPath+'\\config.txt'):
            f=open(self.LauncherPath+'\\config.txt','r')
            exec(f.readline())  #PythonPath
            exec(f.readline())  #PythonwinPath
            exec(f.readline())  #PylibPath
            exec(f.readline())  #DescrPath
            exec(f.readline())  #ProjPath
            exec(f.readline())  #NSIMSDevDir
            exec(f.readline())  #NSIMSVCDir
            exec(f.readline())  #NSIDir
            exec(f.readline())  #MtlbInstall
            exec(f.readline())  #Stimulus
            exec(f.readline())  #IssInstall
            exec(f.readline())  #ReportSpil
            f.close()
        f=open(self.LauncherPath+'\\path.py','w')
        f.write("LauncherPath=r'"+self.LauncherPath+"'")
        f.close()

        #creazione frame
        wxFrame.__init__(self, NULL, -1,'TEST LAUNCHER vs 4.0.18')
        self.Maximize(1)    #massimizzo il frame
        self.ScreenWidth,self.ScreenHeight=self.GetSizeTuple()  #Larghezza e Altezza dello schermo
        self.MngThr=threading.Event() #oggetto utile per gestire i thread
        self.MngThr1=threading.Event() #oggetto utile per gestire i thread
        self.MngThr2=threading.Event() #oggetto utile per gestire i thread
        #creazione di scroll oriz e vert nel frame
        self.scmain=wxScrolledWindow(self, -1,pos = wxDefaultPosition, size = wxDefaultSize, style = wxHSCROLL | wxVSCROLL, name = "scrolledWindow")
        self.scmain.SetScrollbars(10, 10, 106, 70, xPos = 0, yPos = 0, noRefresh = FALSE)
        self.wnd=wxWindow(self.scmain, -1, (0,10), (200,self.ScreenHeight*1.33), wxSUNKEN_BORDER)
        #metto icona sulla barra
        self.SetIcon(wxIcon('launcher.ico',wxBITMAP_TYPE_ICO))

        #creazioni di tutti gli oggetti grafici
        
        if len(glob.glob(self.ProjPath))>0:
            self.CurrentPrj=glob.glob(self.ProjPath)[0] #setto il primo progetto trovato come progetto corrente da aprire
        else:
            self.CurrentPrj=''
        ProjectCombo(self)
        ProjectPath(self)   #setta come corrente la directory relativa al progetto selezionato
        self.TD_DATA=TdParse(self.CurrentDescrPath)[0] #faccio la ricerca dei TD sulla directory corrente
        self.TD_NumTdXInstr=TdParse(self.CurrentDescrPath)[1] #Numero TD X strumento
        SelectorFrame(self) 
        LauncherFrame(self)
        TdBrowse(self)
        ReloadButton(self)
        DevelopUtility(self)
        MenuBar(self)



        

############################################################################################################
# METHOD NAME: OnBrowse 
# METHOD DESCRIPTION: Evento del tasto di browse. Gestisce le finestre del browse presenti nel menubar
#                     per la configurazione del launcher
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al pulsante di browse
#
############################################################################################################
    def OnBrowse (self, event):
        #browse per il python.exe(1) e pythonwin.exe(2)
        if (event.GetId() == 1 or event.GetId() == 2):
            self.wnd.dlg = wxFileDialog(self.PyConfigFrame.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.*", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                if event.GetId() == 1:
                    self.PyConfigFrame.Panel.PythonText.SetValue(self.wnd.dlg.GetPath())
                if event.GetId() == 2:
                    self.PyConfigFrame.Panel.PythonwinText.SetValue(self.wnd.dlg.GetPath())
                    
        #browse per il path della libreria del python
        if event.GetId() == 3:
            self.wnd.dlg = wxDirDialog(self.GenConfigFrame.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.GenConfigFrame.Panel.PylibText.SetValue(self.wnd.dlg.GetPath())

        #browse per il file descrittore della TD lay_descr.txt
        if event.GetId() == 5 :
            self.wnd.dlg = wxFileDialog(self.GenConfigFrame.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.*", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                if event.GetId() == 5:
                    self.GenConfigFrame.Panel.DescrText.SetValue(self.wnd.dlg.GetPath())
                    
        #browse per il path del progetto
        if event.GetId() == 7:
            self.wnd.dlg = wxDirDialog(self.GenConfigFrame.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.GenConfigFrame.Panel.ProjText.SetValue(self.wnd.dlg.GetPath())

        #browse per MSDevDir
        if event.GetId() == 117:
            self.wnd.dlg = wxDirDialog(self.NSIConfigFrame.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.NSIConfigFrame.Panel.MSDevDirText.SetValue(self.wnd.dlg.GetPath())

        #browse per MSVCDir
        if event.GetId() == 118:
            self.wnd.dlg = wxDirDialog(self.NSIConfigFrame.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.NSIConfigFrame.Panel.MSVCDirText.SetValue(self.wnd.dlg.GetPath())

        #browse per NSIDir
        if event.GetId() == 123:
            self.wnd.dlg = wxDirDialog(self.NSIConfigFrame.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.NSIConfigFrame.Panel.NSIDirText.SetValue(self.wnd.dlg.GetPath())

        #browse per l'exe del matlab
        if event.GetId() == 124 :
            self.wnd.dlg = wxFileDialog(self.SPILConfigFrame.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.*", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.SPILConfigFrame.Panel.MtlbInstallText.SetValue(self.wnd.dlg.GetPath())

        #browse per lo stimulus editor
        if event.GetId() == 125 :
            self.wnd.dlg = wxFileDialog(self.SPILConfigFrame.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.exe", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.SPILConfigFrame.Panel.StimulusText.SetValue(self.wnd.dlg.GetPath())

        #browse per l'exe dell'ISS
        if event.GetId() == 126 :
            self.wnd.dlg = wxFileDialog(self.SPILConfigFrame.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.*", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.SPILConfigFrame.Panel.IssInstallText.SetValue(self.wnd.dlg.GetPath())

        #browse per la directory del report
        if event.GetId() == 128 :
            self.wnd.dlg = wxDirDialog(self.SPILConfigFrame.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.SPILConfigFrame.Panel.ReportSpilText.SetValue(self.wnd.dlg.GetPath())

        #browse per l'output dello SPIL
        if event.GetId() == 133:
            self.wnd.dlg = wxFileDialog(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.*", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutTxtText.SetValue(self.wnd.dlg.GetPath())
                
        #browse per il DataDictionary dello SPIL
        if event.GetId() == 134:
            self.wnd.dlg = wxFileDialog(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.*", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.DDText.SetValue(self.wnd.dlg.GetPath())

        #browse per il file delle tolleranze dello SPIL
        if event.GetId() == 135:
            self.wnd.dlg = wxFileDialog(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, message = "", defaultDir = "", defaultFile = "",wildcard = "*.*", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutNameText.SetValue(self.wnd.dlg.GetPath())

        #browse per l'output .mat dello SPIL
        if event.GetId() == 136:
            self.wnd.dlg = wxDirDialog(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.SdtNameText.SetValue(self.wnd.dlg.GetPath())

        #browse per la directory contenente gli stimoli
        if event.GetId() == 144 :
            self.wnd.dlg = wxDirDialog(self.wnd.Develop.Spil.Stim2SdtDlg.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.wnd.Develop.Spil.Stim2SdtDlg.Panel.FolderStimulusText.SetValue(self.wnd.dlg.GetPath())

        #browse per la directory contenente gli stimoli
        if event.GetId() == 154 :
            self.wnd.dlg = wxDirDialog(self.wnd.Develop.Spil.Stb2SdtDlg.Panel, message = "", defaultPath = "", style = wxOPEN)
            if self.wnd.dlg.ShowModal() == wxID_OK:
                self.wnd.Develop.Spil.Stb2SdtDlg.Panel.FolderStimulusText.SetValue(self.wnd.dlg.GetPath())



        self.wnd.dlg.Destroy()



############################################################################################################
# METHOD NAME: SaveConfig 
# METHOD DESCRIPTION: Evento del tasto Save Config per il salvataggio della configurazione inserita tramite
#                     menu bar
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al pulsante di Save Config
#
############################################################################################################
    def SaveConfig(self, event):
        #salvataggio ottenuto premendo il save dentro all' InstrumentsSet menu
        if event.GetId() == 4 :
            self.PythonPath=self.PyConfigFrame.Panel.PythonText.GetValue()
            self.PythonwinPath=self.PyConfigFrame.Panel.PythonwinText.GetValue()
            # se è spuntato il box per la creazione del NS allora chiama  GetNS  
            if self.PyConfigFrame.Panel.CreateNSBox.GetValue():
                self.ThCreateNS=threading.Thread(target=GetNS,args=(self,))
                self.ThCreateNS.start()
            self.PyConfigFrame.Destroy()
            self.PyConfigFrame.Show(0)
            
        #salvataggio ottenuto premendo il save dentro al GeneralSet menu
        if event.GetId() == 6:
            self.DescrPath=self.GenConfigFrame.Panel.DescrText.GetValue()
            self.ProjPath=self.GenConfigFrame.Panel.ProjText.GetValue()
            self.PylibPath=self.GenConfigFrame.Panel.PylibText.GetValue()
            self.GenConfigFrame.Destroy()
            self.GenConfigFrame.Show(0)

        #salvataggio ottenuto premendo il save dentro al InstrumentsSet menu dell'NSI
        if event.GetId() == 119:
            self.NSIMSDevDir=self.NSIConfigFrame.Panel.MSDevDirText.GetValue()
            self.NSIMSVCDir=self.NSIConfigFrame.Panel.MSVCDirText.GetValue()            
            self.NSIDir=self.NSIConfigFrame.Panel.NSIDirText.GetValue()            
            self.NSIConfigFrame.Destroy()
            self.NSIConfigFrame.Show(0)

        #salvataggio ottenuto premendo il save dentro al InstrumentsSet menu dell'SPIL
        if event.GetId() == 127:
            self.MtlbInstall=self.SPILConfigFrame.Panel.MtlbInstallText.GetValue()
            self.Stimulus=self.SPILConfigFrame.Panel.StimulusText.GetValue()            
            self.IssInstall=self.SPILConfigFrame.Panel.IssInstallText.GetValue()
            self.ReportSpil=self.SPILConfigFrame.Panel.ReportSpilText.GetValue()            
            self.SPILConfigFrame.Destroy()
            self.SPILConfigFrame.Show(0)

        #scrittura del file di config.txt
        f=open(self.LauncherPath+'\\config.txt','w+')
        f.write('self.PythonPath=r"'+self.PythonPath+'"')
        f.write('\nself.PythonwinPath=r"'+self.PythonwinPath+'"')
        f.write('\nself.PylibPath=r"'+self.PylibPath+'"')
        f.write('\nself.DescrPath=r"'+self.DescrPath+'"')
        f.write('\nself.ProjPath=r"'+self.ProjPath+'"')
        f.write('\nself.NSIMSDevDir=r"'+self.NSIMSDevDir+'"')
        f.write('\nself.NSIMSVCDir=r"'+self.NSIMSVCDir+'"')
        f.write('\nself.NSIDir=r"'+self.NSIDir+'"')
        f.write('\nself.MtlbInstall=r"'+self.MtlbInstall+'"')
        f.write('\nself.Stimulus=r"'+self.Stimulus+'"')
        f.write('\nself.IssInstall=r"'+self.IssInstall+'"')
        f.write('\nself.ReportSpil=r"'+self.ReportSpil+'"')
        f.close()

       

############################################################################################################
# METHOD NAME: EvtCreateSdt 
# METHOD DESCRIPTION: Evento scatenato dal pulsante di creazione dell'output sdt partendo dalla reg. SPIL
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al pulsante di Save Config
#
############################################################################################################
    def EvtCreateSdt(self, event):
        self.ThCreateSdt=threading.Thread(target=CreateSdt,args=(self,))
        self.ThCreateSdt.start()

############################################################################################################
# METHOD NAME: EvtGenerateSdt 
# METHOD DESCRIPTION: Evento scatenato dal pulsante di generazione di SDT pertendo dallo stimolo
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al pulsante di Save Config
#
############################################################################################################
    def EvtGenerateSdt(self, event):
        self.ThGenerateSdt=threading.Thread(target=GenerateSdt,args=(self,))
        self.ThGenerateSdt.start()

############################################################################################################
# METHOD NAME: EvtStb2Sdt 
# METHOD DESCRIPTION: Evento scatenato dal pulsante di generazione di SDT partendo da STB
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al pulsante di Save Config
#
############################################################################################################
    def EvtStb2Sdt(self, event):
        self.ThStb2Sdt=threading.Thread(target=Stb2Sdt,args=(self,))
        self.ThStb2Sdt.start()


############################################################################################################
# METHOD NAME: EvtChoice 
# METHOD DESCRIPTION: Evento del TdBrowse. In base alla selezione fatta, la finestra del SelectorFrame si sposta
#                     orizzontalmente di un tot agevolando la ricerca del TD
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al listbox del TdBrowse
#
############################################################################################################
    def EvtChoice(self, event):
        self.scmain.sc.Scroll(event.GetSelection()*20,0)


############################################################################################################
# METHOD NAME: EvtReload 
# METHOD DESCRIPTION: Evento del pulsante di RELOAD. Ricostruisce la finestra del SelectorFrame rianalizzando il
# path relativo al progetto selezionato
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al pulsante RELOAD
#
############################################################################################################
    def EvtReload(self, event):
        #ricostruisce la lista del combo relativo al progetto in quanto potrebbe essere cambiata
        ProjectPath(self)
        self.wnd.Project.Destroy()
        ProjectCombo(self)
        
        #ricostruisce il SelectorFrame 
        self.scmain.sc.Destroy()
        self.TD_DATA=TdParse(self.CurrentDescrPath)[0]
        self.TD_NumTdXInstr=TdParse(self.CurrentDescrPath)[1] #Numero TD X strumento
        self.wnd.launcher.ch.Destroy()
        TdBrowse(self)
        SelectorFrame(self)



############################################################################################################
# METHOD NAME: EvtTcAll 
# METHOD DESCRIPTION: Eventi di tutti i pulsanti di NO OL CL. Setta tutti i radiobutton al valore impostato
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato ai pulsanti di NO OL CL
#
############################################################################################################
    def EvtTcAll(self, event):
        #Gli Id dei pulsanti sono:
        #NO globale = 0
        #OL globale = 1
        #CL globale = 2
        #NO locale =x0 dove x è la TD relativa
        #OL locale =x1 dove x è la TD relativa
        #CL locale =x2 dove x è la TD relativa 
        ev=str(event.GetId())
        TipoSel=ev[len(ev)-1]   #la selezione è rappresenta sempre dalla cifra meno significativat
        TD=ev[0:len(ev)-1]      #il numero di TD è rappresentato dalle restanti cifrea più significativeve
        #se TD='' significa che è quello globale e perciò faccio il for su tutti i TD  
        if TD=='':
            for indexTD in range(len(self.TD_DATA)):
                TD=str(indexTD+1)
                GeneralSel(self,TD,TipoSel)
        else:
            GeneralSel(self,TD,TipoSel)

############################################################################################################
# METHOD NAME: EvtGO 
# METHOD DESCRIPTION: Al premere del GO raccolgo tutte le selezioni dei TC e lancio il RUN e il controllore di led
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato al GO
#
############################################################################################################
    def EvtGO(self, event): 
        self.GlobalTestList=[]
        for TD in range(len(self.TD_DATA)): 
            exec('TypeTestListTD'+str(TD+1)+'=[]')
            for TC in range(len(self.TD_DATA[TD][2])-1):
                str2exe="TypeTestListTD"+str(TD+1)+".append(self.scmain.sc.TD"+str(TD+1)+".rb"+str(TC+1)+"_"+str(TD+1)+".GetSelection())"
                exec(str2exe)
            str2exe="self.GlobalTestList.append(TypeTestListTD"+str(TD+1)+")"
            exec(str2exe)
            
        #se non è stato selezionato nessun TC non faccio partire il RUN_TESTl
        NoTest=1
        for TypeTestListTD in self.GlobalTestList:
            if TypeTestListTD.count(1) or TypeTestListTD.count(2):
                NoTest=0
                break
            else:            
                NoTest=1
        if not(NoTest):
            #parte il thread che controlla il file dei led
            self.ThLed=threading.Thread(target=CheckLed,args=(self,))
            self.ThLed.start()
            #parte il thread che lancia i launcher dei vari strumenti
            ThRun=threading.Thread(target=RUN_TEST,args=(self,))
            ThRun.start()


############################################################################################################
# FUNCTION NAME : RptTest
# FUNCTION DESCRIPTION: Incremento del TextControl x ripetizione test
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente
#           2) evento
#
############################################################################################################
    def RptTest(self,event):
        self.wnd.RptTestTxt.SetValue(str(event.GetPosition()))
    


############################################################################################################
# METHOD NAME: Configuration 
# METHOD DESCRIPTION: gestione eventi menu configurazione
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: eventi legati al menu
#
############################################################################################################
    def Configuration(self,event):
        #menu Instruments
        if event.GetId() == 105:
            PyConfig(self)
        #menu TD set
        if event.GetId() == 108:
            GenConfig(self)
        #menu NSI set
        if event.GetId() == 106:
            NSIConfig(self)
        #menu SPIL set
        if event.GetId() == 107:
            SPILConfig(self)
        #menu Exit set
        if event.GetId() == 156:
            ExitProc(self)


############################################################################################################
# METHOD NAME: EvtPyUtility
# METHOD DESCRIPTION: Evento dei pulsanti di Utility Python
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato ai pulsanti del python utility
#
############################################################################################################
    def EvtPyUtility(self, event):
        #Setto il path (in un thread separato con win98 non necessario)
        if event.GetId() == 113:
            self.ThSetPath=threading.Thread(target=SetPath,args=(self,))
            self.ThSetPath.start()

        #Cancello i moduli importati (in un thread separato con win98 non necessario)
        if event.GetId() == 114:
            self.ThClrNS=threading.Thread(target=ClrNS,args=(self,))
            self.ThClrNS.start()

        #Invoco il ChangeStimolo
        if event.GetId() == 115:           
            #creazione frame generale
            self.wnd.Develop.Python.ChStimDlg=wxMiniFrame(None, -1,title="Change Stimulus", pos=(200,100), size=(500,300), style=wxDEFAULT_FRAME_STYLE)
            self.wnd.Develop.Python.ChStimDlg.Panel = wxPanel(self.wnd.Develop.Python.ChStimDlg, -1,pos=(0,0), size=(500,300))
            self.wnd.Develop.Python.ChStimDlg.Panel.Browse=wxGenericDirCtrl(self.wnd.Develop.Python.ChStimDlg.Panel, -1, dir=self.CurrentPrj,size=(200,200), style=wxDIRCTRL_3D_INTERNAL|wxDIRCTRL_SHOW_FILTERS|wxMULTIPLE,filter="Python files (*.py)|*.py")
            self.wnd.Develop.Python.ChStimDlg.Panel.Start =wxButton(self.wnd.Develop.Python.ChStimDlg.Panel, 116, "START CHANGE",pos=(250,20),size=(200,30))
            EVT_BUTTON(self.wnd.Develop.Python.ChStimDlg.Panel.Start, 116, self.EvtChgStim)
            self.wnd.Develop.Python.ChStimDlg.Panel.Check= wxCheckBox(self.wnd.Develop.Python.ChStimDlg.Panel, -1,   "Recurse Sub-Directory", wxPoint(50, 210), wxSize(150, 20), wxNO_BORDER)
            self.wnd.Develop.Python.ChStimDlg.Show(1)

############################################################################################################
# METHOD NAME: EvtSpilUtility
# METHOD DESCRIPTION: Evento dei pulsanti di Utility Spil
# 
# INPUT PARAMETERS: 
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#           2) event: evento legato ai pulsanti del python utility
#
############################################################################################################
    def EvtSpilUtility(self, event):
        #Invoco il Convert2Sdt per convertire il prodotto dello SPIL .txt in formato sdt .mat
        if event.GetId() == 129:           
            #creazione frame generale
            self.wnd.Develop.Spil.Cv2SdtDlg=wxMiniFrame(None, -1,title="Convert2Sdt", pos=(200,100), size=(500,300), style=wxDEFAULT_FRAME_STYLE)
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel = wxPanel(self.wnd.Develop.Spil.Cv2SdtDlg, -1,pos=(0,0), size=(500,300))
            #creazione text control e browse di Output.txt
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutTxtLabel=wxStaticText(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "Output.txt:",pos=(5,7), size=(95,20),style=wxALIGN_RIGHT)
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutTxtText = wxTextCtrl(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "",pos=(100,5), size=(300, 20))
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutTxtBrowse =wxButton(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, 133, "...",pos=(405,5),size=(18,20))
            EVT_BUTTON(self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutTxtBrowse, 133, self.OnBrowse)
            #creazione text control e browse di DataDict.mat
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.DDLabel=wxStaticText(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "DataDict.mat:",pos=(5,47), size=(95,20),style=wxALIGN_RIGHT)
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.DDText = wxTextCtrl(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "",pos=(100,45), size=(300, 20))
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.DDBrowse =wxButton(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, 134, "...",pos=(405,45),size=(18,20))
            EVT_BUTTON(self.wnd.Develop.Spil.Cv2SdtDlg.Panel.DDBrowse, 134, self.OnBrowse)
            #creazione text control e browse di Toll.m
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutNameLabel=wxStaticText(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "TOLL_xxx_GEN.m:",pos=(5,87), size=(95,20),style=wxALIGN_RIGHT)
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutNameText = wxTextCtrl(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "",pos=(100,85), size=(300, 20))
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutNameBrowse =wxButton(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, 135, "...",pos=(405,85),size=(18,20))
            EVT_BUTTON(self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutNameBrowse, 135, self.OnBrowse)
            #creazione text control e browse dell'SDT di Output.mat
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.SdtNameLabel=wxStaticText(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "File da creare .mat:",pos=(5,127), size=(95,20),style=wxALIGN_RIGHT)
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.SdtNameText = wxTextCtrl(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, -1, "",pos=(100,125), size=(300, 20))
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.SdtNameBrowse =wxButton(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, 136, "...",pos=(405,125),size=(18,20))
            EVT_BUTTON(self.wnd.Develop.Spil.Cv2SdtDlg.Panel.SdtNameBrowse, 136, self.OnBrowse)
            #creazione pulsante salvataggio
            self.wnd.Develop.Spil.Cv2SdtDlg.Panel.CreateSdt=wxButton(self.wnd.Develop.Spil.Cv2SdtDlg.Panel, 137,'Create SdtOut',pos=(150,200),size=(200,40))
            EVT_BUTTON(self.wnd.Develop.Spil.Cv2SdtDlg.Panel.CreateSdt,137,self.EvtCreateSdt)
            self.wnd.Develop.Spil.Cv2SdtDlg.Show(1)
        #Invoco l'apertura dello stimulus editor x SPIL
        if event.GetId() == 139:           
            self.ThOpenStimulus=threading.Thread(target=OpenStimulus,args=(self,))
            self.ThOpenStimulus.start()
        #Invoco l'elaborazione delle tracce ricavate dello stimous editor x ottenere l'SDT
        if event.GetId() == 143:
            #creazione frame generale
            self.wnd.Develop.Spil.Stim2SdtDlg=wxMiniFrame(None, -1,title="Sdt Generator", pos=(200,100), size=(500,200), style=wxDEFAULT_FRAME_STYLE)
            self.wnd.Develop.Spil.Stim2SdtDlg.Panel = wxPanel(self.wnd.Develop.Spil.Stim2SdtDlg, -1,pos=(0,0), size=(500,200))
            #creazione text control e browse x la cartella contenente gli stimoli
            self.wnd.Develop.Spil.Stim2SdtDlg.Panel.FolderStimulusLabel=wxStaticText(self.wnd.Develop.Spil.Stim2SdtDlg.Panel, -1, "Stimulus Folder:",pos=(5,37), size=(95,20),style=wxALIGN_RIGHT)
            self.wnd.Develop.Spil.Stim2SdtDlg.Panel.FolderStimulusText = wxTextCtrl(self.wnd.Develop.Spil.Stim2SdtDlg.Panel, -1, "",pos=(100,35), size=(300, 20))
            self.wnd.Develop.Spil.Stim2SdtDlg.Panel.FolderStimulusBrowse =wxButton(self.wnd.Develop.Spil.Stim2SdtDlg.Panel, 144, "...",pos=(405,35),size=(18,20))
            EVT_BUTTON(self.wnd.Develop.Spil.Stim2SdtDlg.Panel.FolderStimulusBrowse, 144, self.OnBrowse)
            #creazione text control per lo step dello stimolo
            self.wnd.Develop.Spil.Stim2SdtDlg.Panel.StimulusStepLabel=wxStaticText(self.wnd.Develop.Spil.Stim2SdtDlg.Panel, -1, "Stimulus Step [sec]:",pos=(5,77), size=(95,20),style=wxALIGN_RIGHT)
            self.wnd.Develop.Spil.Stim2SdtDlg.Panel.StimulusStepText = wxTextCtrl(self.wnd.Develop.Spil.Stim2SdtDlg.Panel, -1, "0.001",pos=(100,75), size=(50, 20))
            #creazione pulsante per il processing
            self.wnd.Develop.Spil.Stim2SdtDlg.Panel.CreateSdt=wxButton(self.wnd.Develop.Spil.Stim2SdtDlg.Panel, 145,'Converting in Sdt format',pos=(150,120),size=(200,40))
            EVT_BUTTON(self.wnd.Develop.Spil.Stim2SdtDlg.Panel.CreateSdt,145,self.EvtGenerateSdt)
            self.wnd.Develop.Spil.Stim2SdtDlg.Show(1)
        #Invoco l'elaborazione delle tracce ricavate da STB x ottenerle in formato SDT
        if event.GetId() == 153:
            #creazione frame generale
            self.wnd.Develop.Spil.Stb2SdtDlg=wxMiniFrame(None, -1,title="Sdt Generator", pos=(200,100), size=(500,200), style=wxDEFAULT_FRAME_STYLE)
            self.wnd.Develop.Spil.Stb2SdtDlg.Panel = wxPanel(self.wnd.Develop.Spil.Stb2SdtDlg, -1,pos=(0,0), size=(500,200))
            #creazione text control e browse x la cartella contenente gli stimoli
            self.wnd.Develop.Spil.Stb2SdtDlg.Panel.FolderStimulusLabel=wxStaticText(self.wnd.Develop.Spil.Stb2SdtDlg.Panel, -1, "Stimulus Folder:",pos=(5,37), size=(95,20),style=wxALIGN_RIGHT)
            self.wnd.Develop.Spil.Stb2SdtDlg.Panel.FolderStimulusText = wxTextCtrl(self.wnd.Develop.Spil.Stb2SdtDlg.Panel, -1, "",pos=(100,35), size=(300, 20))
            self.wnd.Develop.Spil.Stb2SdtDlg.Panel.FolderStimulusBrowse =wxButton(self.wnd.Develop.Spil.Stb2SdtDlg.Panel, 154, "...",pos=(405,35),size=(18,20))
            EVT_BUTTON(self.wnd.Develop.Spil.Stb2SdtDlg.Panel.FolderStimulusBrowse, 154, self.OnBrowse)
            #creazione pulsante per il processing
            self.wnd.Develop.Spil.Stb2SdtDlg.Panel.CreateSdt=wxButton(self.wnd.Develop.Spil.Stb2SdtDlg.Panel, 155,'Converting in Sdt format',pos=(150,120),size=(200,40))
            EVT_BUTTON(self.wnd.Develop.Spil.Stb2SdtDlg.Panel.CreateSdt,155,self.EvtStb2Sdt)
            self.wnd.Develop.Spil.Stb2SdtDlg.Show(1)


############################################################################################################
# FUNCTION NAME : EvtChgStim
# FUNCTION DESCRIPTION: Chiama lo script ClearNS.py
#                          - Cancella i moduli importati e le variabili
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
    def EvtChgStim(self,event):
        self.ThChgStim=threading.Thread(target=ChgStim,args=(self,))
        self.ThChgStim.start()



############################################################################################################
# FUNCTION NAME : EvtLog
# FUNCTION DESCRIPTION: Apre il file di log
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
    def EvtLog(self,event):
        os.startfile(r'log.txt')




############################################################################################################
# CLASS NAME:   App 
# CLASS DESCRIPTION: Costrutto base per  le interfaccie grafiche
#
# PARAMETERS: eventuali parametri da passare al creatore della classe
#           1) wxApp
#
# LIST OF METHODS: OnInit
############################################################################################################
class App(wxApp):
    def OnInit(self):
        frame=TdSelectorLay()
        frame.Show(true)
        return true



############################################################################################################
# FUNCTION NAME : GeneralSel
# FUNCTION DESCRIPTION:  
# E' la funzione che materialmente setta i radiobutton al valore impostato dai pulsanti NO OL CL
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente
#           2) TD: stringa indicante il numero della TD su cui agire
#           3) TipoSel: stringa indicante il tipo di settaggio da effettuare sui radiobutton
#
############################################################################################################
def GeneralSel(self,TD,TipoSel):
    str2exe="TcNum=len(self.TD_DATA["+TD+"-1][2])-1"
    exec(str2exe)
    for indexTC in range(TcNum):
        str2exe="self.scmain.sc.TD"+TD+".rb"+str(indexTC+1)+"_"+TD+".SetSelection("+TipoSel+")"
        exec(str2exe)




############################################################################################################
# FUNCTION NAME : ProjectPath
# FUNCTION DESCRIPTION:  
# Setta il path per i descrittori di TD e per le librerie in funzione al progetto selezionato
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente
#
############################################################################################################
def ProjectPath(self):
    self.CurrentPrj=r''+self.wnd.Project.GetValue() #progetto selezionato
    lensplitPrj=len(string.split(self.CurrentPrj,'\\')) 
    splitPath=string.split(self.DescrPath,'\\')
    #il path lo compongo partendo dal path del progetto selezionato e aggiungendo le parti del DescrPath rimanenti
    self.CurrentDescrPath=self.CurrentPrj 
    for i in range(lensplitPrj,len(splitPath)):
        self.CurrentDescrPath=self.CurrentDescrPath+'\\'+splitPath[i]
    #il path lo compongo partendo dal path del progetto selezionato e aggiungendo le parti del PylibPath rimanenti
    self.CurrentPylibPath=self.CurrentPrj
    splitLib=string.split(self.PylibPath,'\\')
    for i in range(lensplitPrj,len(splitLib)):
        self.CurrentPylibPath=self.CurrentPylibPath+'\\'+splitLib[i]
    if self.CurrentPrj=='':
        self.CurrentDescrPath=''
        self.CurrentPylibPath=''

############################################################################################################
# FUNCTION NAME : CompList
# FUNCTION DESCRIPTION: Utility per le liste del Led_status e Type_List. Se è stato selezionato un TC 
#                       e il led corrispondente non si è acceso  significa che c'è stato un errore .
#
# INPUT PARAMETERS:
#           1) TL lista dei test case selezionati in un TD
#           2) LS lista dello stato dei led relativi
# OUTPUT:
#           1) Ritorna 0 se c'è un'incongruenza (TC selezionato e Led spento) 1 altrimenti 
############################################################################################################
def CompList(TL,LS):
    if (TL!=0) and (LS==0 or LS==None):
        return 0
    else:
        return 1

############################################################################################################
# FUNCTION NAME : CheckLed
# FUNCTION DESCRIPTION: Alla fine dell'esecuzione del TD setta il colore dei led in base del risultato dei TC.
# Funzione lanciata in thread parallelo con il RUN che controlla periodicamente lo stato del file led_status.txt.
# L'accesso in scrittura e lettura di questo file è regolato attraverso un ulteriore file di flag chiamato
# led.flg. Questo flag è creato dal PyTdLnc.py e, in generale, da tutti i lanciatori di strumento 
# al termine di ogni esecuzione di TD e cancellato solo alla fine del settaggio dei led della TD.
# Grazie a questo flag si evita il contemporaneo accesso del file da parte di 2 programmi diversi
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def CheckLed(self):
    #se per qualsiasi motivo alla partenza si ha il file di flag questo deve essere cancellato 
    if (os.path.exists(self.LauncherPath+'\\led.flg')):
        os.remove(self.LauncherPath+'\\led.flg')
    #Aspetto fintantochè non è finita la parte di inizializzazione del file led_status.txt fatta in   
    #RUN_TEST
    self.MngThr2.wait()
    self.Iconize(1) #al lancio dei test mi iconizza il launcher
    stopflg=0   #quando 1 significa che è finita l'esecuzione di tutti i tests di tutti gli strumenti 
    #e posso quindi uscire dalla  funzione
    while not(stopflg):
        #E' il gestore di thread che mette in wait il thread per 2 secondi
        #lasciando più CPU per il thread RUN eseguito in parallelo .
        self.MngThr.wait(2)
        #lettura led solo se esiste il flag
        if (os.path.exists(self.LauncherPath+'\\led.flg')):
            f=open(self.LauncherPath+'\\led_status.txt','r+')
            exec(f.readline())  #LED_STATUS=[[1,2,0,3],[],[2,1]]
            exec(f.readline())  #stopInstr=0
            exec(f.readline())  #stopflgHIL=0
            exec(f.readline())  #stopflgNSI=0
            exec(f.readline())  #stopflgSPIL=0
            #Lo stopflg (quindi la fine del programma) si ha solo quando tutti gli strumenti hanno finito e
            #e sono state eseguite tutte le ripetizioni indicate
            stopflg=stopflgHIL&stopflgNSI&stopflgSPIL&(self.nrpt+1==(int(self.wnd.RptTestSpn.GetValue())))   #se 1 significa fine della sessione di tests
            LedColor=['grey','black','green','red']
            #setto il colore dei led in base al risultato
            for indexTD in range(len(LED_STATUS)):
                if LED_STATUS[indexTD].count(1) or LED_STATUS[indexTD].count(2) or LED_STATUS[indexTD].count(3):    #se per un certo TD non sono stati selezionati TC, lascio in grigio
                    for indexTC in range(len(LED_STATUS[indexTD])):
                        str2exe="self.scmain.sc.TD"+str(indexTD+1)+".led"+str(indexTC+1)+"_"+str(indexTD+1)+".SetBackgroundColour(LedColor["+str(LED_STATUS[indexTD][indexTC])+"])"
                        #es: self.scmain.sc.TD1.led1_1..SetBackgroundColour(LedColor["+str(LED_STATUS[indexTD][indexTC])+"])
                        exec(str2exe)
                        str2exe="self.scmain.sc.TD"+str(indexTD+1)+".Refresh()" #rinfresco la pagina per vedere l'effetto del set
                        #es: self.scmain.sc.TD1.Refresh()
                        exec(str2exe)
                #Verifico anche che i test non abbiano avuto problemi nell'esecuzione.
                #In caso negativo coloro di rosso il pulsante di log.
                if indexTD>0:
                    if map(CompList,self.GlobalTestList[indexTD-1],LED_STATUS[indexTD-1]).count(0)>0:
                        self.wnd.launcher.log.SetBackgroundColour(wxRED)
            #stopInstr è il flag che dice se il test con uno degli strumenti è terminato  
            if (stopInstr==1):
                #Verifico anche che i test non abbiano avuto problemi nell'esecuzione.
                #In caso negativo coloro di rosso il pulsante di log.
                #print "indexTD=",indexTD
                if LED_STATUS!=[]:      #LED_STATUS è uguale a[] in caso di debug  
                    if map(CompList,self.GlobalTestList[indexTD],LED_STATUS[indexTD]).count(0)>0:
                        self.wnd.launcher.log.SetBackgroundColour(wxRED)
                #se la sessione di uno strumento è terminata --> semaforo verde x il thread MngThr1 
                self.MngThr1.set()
                f.seek(0)
                tmpList=f.readlines()
                tmpList[1]='stopInstr=0\n'  #resetto il flag di fine strumento
                f.seek(0)
                f.writelines(tmpList)
                f.close()
            os.remove(self.LauncherPath+'\\led.flg')    #solo alla fine delle mie operazioni rimuovo il flag
    #abilito i pulsanti di GO ed ENABLE disabilitati nella funzione RUN_TEST
                        
    self.wnd.GO.Enable(TRUE)    
    self.wnd.reload.Enable(TRUE)
    self.wnd.RptTestSpn.Enable(TRUE)
    self.wnd.RptTestTxt.Enable(TRUE)

    self.Maximize(1)


############################################################################################################
# FUNCTION NAME : SetPath
# FUNCTION DESCRIPTION: Chiama lo script SetPath.py
#                          - Include nel pythonpath le librerie del progetto e le cartelle dei TD selezionati
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def SetPath(self):
    #Raccolgo i TD selezionati
    self.GlobalTestList=[]
    for TD in range(len(self.TD_DATA)): 
        exec('TypeTestListTD'+str(TD+1)+'=[]')
        for TC in range(len(self.TD_DATA[TD][2])-1):
            str2exe="TypeTestListTD"+str(TD+1)+".append(self.scmain.sc.TD"+str(TD+1)+".rb"+str(TC+1)+"_"+str(TD+1)+".GetSelection())"
            exec(str2exe)
        str2exe="self.GlobalTestList.append(TypeTestListTD"+str(TD+1)+")"
        exec(str2exe)

    #Scrivo nel tmp.txt le informazioni sui tests selezionati, sul nome dei tests e sul path della libreria 
    f=open(self.LauncherPath+'\\tmp.txt','w+')
    f.write('HilTestList='+str(self.GlobalTestList))
    f.write('\nHilTD_DATA='+str(self.TD_DATA))
    f.write('\nPylibPath=r"'+str(self.CurrentPylibPath)+'"')
    f.write('\n')
    f.close()

    if string.lower(os.path.split(self.PythonwinPath)[1])=="idle.pyw":                        
        #command=self.PythonwinPath + " " +self.LauncherPath+'\\SetPath.py'
        command=""
    elif string.lower(os.path.split(self.PythonwinPath)[1])=="pythonwin.exe":
        revpath=string.replace(self.LauncherPath+'\\SetPath.py','\\','/')
        command=self.PythonwinPath+" /run "+revpath
    else:
        print "NOT VALID DEBUGGER\n Change debugger program "




        
    os.system(command)

############################################################################################################
# FUNCTION NAME : ClrNS
# FUNCTION DESCRIPTION: Chiama lo script ClearNS.py
#                          - Cancella i moduli importati e le variabili
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def ClrNS(self):
    if string.lower(os.path.split(self.PythonwinPath)[1])=="idle.pyw":                        
        #command=self.PythonwinPath +' -d -e "'+self.LauncherPath+'\\ClearNS.py"'
        command=""
    elif string.lower(os.path.split(self.PythonwinPath)[1])=="pythonwin.exe":
        revpath=string.replace(self.LauncherPath+'\\ClearNS.py','\\','/')
        command=self.PythonwinPath+" /run "+revpath
    else:
        print "NOT VALID DEBUGGER\n Change debugger program "
    os.system(command)





############################################################################################################
# FUNCTION NAME : GetNS
# FUNCTION DESCRIPTION: Chiama lo script GetNS.py
#                          - Crea il file NStmp in cui vi è il NS di riferimento 
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def GetNS(self):    
    revpath=string.replace(self.LauncherPath+'\\GetNS.py','\\','/')
    command=self.PythonwinPath+" /run "+revpath    
    os.system(command)
    
    
    

############################################################################################################
# FUNCTION NAME : ChgStim
# FUNCTION DESCRIPTION: Chiama lo script change_stim.py
#                          - cambia gli script degli stimoli
#                           tipo=0 cambia file
#                           tipo=1 cambia contenuto cartelle
#                           tipo=2 cambia contenuto cartella e sottocartelle
#
#
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def ChgStim(self):
    if os.path.isdir(self.wnd.Develop.Python.ChStimDlg.Panel.Browse.GetPath()):
        if self.wnd.Develop.Python.ChStimDlg.Panel.Check.GetValue():
            tipo = 2
        else:
            tipo = 1
    else:
        tipo=0
    f=open(self.LauncherPath+'\\tmpchgstim.txt','w+')
    f.write('tipo='+str(tipo))
    f.write('\ntop_path=r"'+self.wnd.Develop.Python.ChStimDlg.Panel.Browse.GetPath()+'"')
    f.close()
    revpath=string.replace(self.LauncherPath+'\\change_stim.py','\\','/')
    command=self.PythonwinPath+" /run "+revpath
    self.wnd.Develop.Python.ChStimDlg.Destroy()
    os.system(command)


############################################################################################################
# FUNCTION NAME : CreateSdt
# FUNCTION DESCRIPTION: Chiama la funzione matlab il matlab GetSdtOut x convertire gli output SPIL in
#                       output SDT
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def CreateSdt(self):
    #SpilMatLib='M:\\Work\\4TV_SPIL_00\\LIB\\SPILMATLIB\n'
    SpilMatLib=os.path.dirname(glob.glob(self.CurrentPylibPath+'\\SetCalib.m')[0]) #OK generale

    LaunchFile=" /r cd('"+SpilMatLib+"');GetSdtOut('"+\
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutTxtText.GetValue()+"','"+\
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.DDText.GetValue()+"','"+\
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.OutNameText.GetValue()+"','"+\
                self.wnd.Develop.Spil.Cv2SdtDlg.Panel.SdtNameText.GetValue()+"');quit"
    os.system(self.MtlbInstall+LaunchFile)
    self.wnd.Develop.Spil.Cv2SdtDlg.Destroy()
    self.wnd.Develop.Spil.Cv2SdtDlg.Show(0)

############################################################################################################
# FUNCTION NAME : OpenStimulus
# FUNCTION DESCRIPTION: Apre lo stimulus editor x lo spil
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def OpenStimulus(self):
    os.system(self.Stimulus)

############################################################################################################
# FUNCTION NAME : GenerateSdt
# FUNCTION DESCRIPTION: Chiama la funzione matlab Stimulus2Sdt x convertire gli stimoli dello stimulus
#                       editor in SDT
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def GenerateSdt(self):
    #SpilMatLib='M:\\Work\\4TV_SPIL_00\\LIB\\SPILMATLIB\n'
    SpilMatLib=os.path.dirname(glob.glob(self.CurrentPylibPath+'\\SetCalib.m')[0]) #OK generale

    LaunchFile=" /r cd('"+SpilMatLib+"');Stimulus2Sdt('"+\
                self.wnd.Develop.Spil.Stim2SdtDlg.Panel.StimulusStepText.GetValue()+"','"+\
                self.wnd.Develop.Spil.Stim2SdtDlg.Panel.FolderStimulusText.GetValue()+"');quit"
    os.system(self.MtlbInstall+LaunchFile)
    self.wnd.Develop.Spil.Stim2SdtDlg.Destroy()
    self.wnd.Develop.Spil.Stim2SdtDlg.Show(0)

############################################################################################################
# FUNCTION NAME : Stb2Sdt
# FUNCTION DESCRIPTION: Chiama la funzione matlab Stb2Sdt x convertire i pattern di STB
#                       in formato SDT
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def Stb2Sdt(self):
    #SpilMatLib='M:\\Work\\4TV_SPIL_00\\LIB\\SPILMATLIB\n'
    SpilMatLib=os.path.dirname(glob.glob(self.CurrentPylibPath+'\\SetCalib.m')[0]) #OK generale

    LaunchFile=" /r cd('"+SpilMatLib+"');Stb2Sdt('"+\
                self.wnd.Develop.Spil.Stb2SdtDlg.Panel.FolderStimulusText.GetValue()+"');quit"
    os.system(self.MtlbInstall+LaunchFile)
    self.wnd.Develop.Spil.Stb2SdtDlg.Destroy()
    self.wnd.Develop.Spil.Stb2SdtDlg.Show(0)


############################################################################################################
# FUNCTION NAME : ClearPyc
# FUNCTION DESCRIPTION: Funzione che cancella i file .pyc se trova nella stessa cartella file omonimi
#                       ma con estensione .py. La ricerca vieve effettuata nelle librerie e nei TD
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def ClearPyc(self):    
    print "Deleting .pyc files..."
    # Trovo i files pyc presenti nei TD, nelle Librerie e sotto Work
    PycList=glob.glob(self.CurrentPrj+'\\*\\*\\*.pyc')+glob.glob(os.path.split(self.ProjPath)[0]+'\\*.pyc')
    # Solo se esiste un corrispondente .py rimuovo il .pyc
    for Pyc in PycList:
        if os.path.exists(os.path.splitext(Pyc)[0]+'.py'):
            os.remove(Pyc)      


############################################################################################################
# FUNCTION NAME : RUN_TEST
# FUNCTION DESCRIPTION: Chiama lo script di lancio PyTdLnc x i test HIL e lancia direttamente i test x NSI e SPIL
#                       in modalità RUN o DEBUGU (può chiamare qualsiasi script anche non python)  
#                          - Disabilita i pulsanti di GO e RELOAD
#                          - Crea un file per il log 
#                          - Resetta lo stato dei led
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def RUN_TEST(self):
    #Se il lancio è stato fatto in debug non considerare i repeat  
    if self.wnd.launcher.run.GetSelection():        
        self.wnd.RptTestSpn.SetValue(1)
    #Ripeto lo scenario di test quante volte indicato dal pannello        
    for self.nrpt in range(int(self.wnd.RptTestSpn.GetValue())):
        if int(self.wnd.RptTestSpn.GetValue())>1:
            print "\n\n\nRIPETIZIONE SCENARIO N° ",self.nrpt+1
            NTS=str(self.nrpt+1)+'°' 
        else:
            NTS=''        

        #inizializzo il file led_status.txt con i stopflag degli strumenti = 0
        f=open('led_status.txt','w')
        f.writelines(['LED_STATUS=[]\n','stopInstr=0\n','stopflgHIL=0\n','stopflgNSI=0\n','stopflgSPIL=0\n'])
        f.close()
        #Dopo l'inizializzazione, in cui ho lavorato sul file led_status.txt, posso dare il verde alla CheckLed
        #(Se non lo facessi ci sarebbe il rischio di una collisione sul led_status.txt)
        self.MngThr2.set()

        #Mi assicuro di essere nella directory del Launcher        
        os.chdir(self.LauncherPath)
        #Metto il semaforo rosso a questo Thread (starò in wait finchè  il singolo strumento non ha finito)  
        self.MngThr1.clear()
        #Disabilita i pulsanti di GO, RELOAD e repeat 
        self.wnd.GO.Enable(FALSE)
        self.wnd.reload.Enable(FALSE)
        self.wnd.RptTestSpn.Enable(FALSE)
        self.wnd.RptTestTxt.Enable(FALSE)
        #Crea un file per il log a cui i singoli TD devono accedere con l'attributo append
        #Inizialmente ha colore verde, se vi è qualche errore diventa rosso 
        self.wnd.launcher.log.SetBackgroundColour(wxGREEN)
        if self.nrpt==0:
            filelog=open(self.LauncherPath+'\\log.txt','w')
        else:
            filelog=open(self.LauncherPath+'\\log.txt','a')
        filelog.write("\n\n\nSTART %s TESTS SESSION:%s\n"%(NTS,time.ctime(time.time())))
        filelog.close()
        #Resetto lo stato dei led
        for indexTD in range(len(self.GlobalTestList)):
            for indexTC in range(len(self.GlobalTestList[indexTD])):
                str2exe="self.scmain.sc.TD"+str(indexTD+1)+".led"+str(indexTC+1)+"_"+str(indexTD+1)+".SetBackgroundColour('grey')"
                exec(str2exe)

        #Leggo se lanciare il test per ecu sviluippo piuttosto che ecu di produzione
        if not(self.wnd.launcher.ECU.GetSelection()):
            self.EcuType="EcuType='sviluppo'"
        else:
            self.EcuType="EcuType='produzione'"
        #Verifico strumento x strumento se sono stati selezionati dei TC
        InstrExec=[]    #InstrExec ha tanti elementi quanti sono gli strumenti e sono settati a 1 se c'è almeno un TC selezionato
        for nInstr in range(1,len(self.TD_NumTdXInstr)):
            InstrExec.append(0)
            #scandisco il GlobalTestList solo nel range dello strumento considerato
            for TypeTestListTD in self.GlobalTestList[self.TD_NumTdXInstr[nInstr-1]:self.TD_NumTdXInstr[nInstr]]:
                if TypeTestListTD.count(1) or TypeTestListTD.count(2):
                    InstrExec[nInstr-1]=1
                    break


    #----------------------------LANCIO I TEST PER HIL (solo se almeno 1 TC è selezionato) ---------------------------# 
        if InstrExec[0]==1:
            #Scrivo nel tmp.txt le informazioni sui tests selezionati, sul nome dei tests e sul path della libreria utili per il PyTdLnc.py
            f=open(self.LauncherPath+'\\tmp.txt','w+')
            f.write(self.EcuType)
            f.write('\nHilTestList='+str(self.GlobalTestList[:self.TD_NumTdXInstr[1]]))
            f.write('\nHilTD_DATA='+str(self.TD_DATA[:self.TD_NumTdXInstr[1]]))
            f.write('\nPylibPath=r"'+str(self.CurrentPylibPath)+'"')
            f.write('\n')
            f.close()
            try:
                # Se c'è il flag di cancellatura .pyc,  lancio la funzione  che  dalla root
                # elimina i pyc se esiste anche il py (sia nelle lib che nei TD)
                if self.wnd.Develop.Python.PycBox.GetValue():
                    ClearPyc(self)                
                             
                #modalità RUN
                if not(self.wnd.launcher.run.GetSelection()):
                    command=self.PythonPath+' -c "import PyTdLnc"'
                    os.system(command)
                #modalità DEBUG    
                else:
                    if string.lower(os.path.split(self.PythonwinPath)[1])=="idle.pyw":                        
                        command=self.PythonwinPath +' -d -e "'+self.LauncherPath+'\\PyTdLnc.py"'
                        print"debug session..."
                        os.system(command)
                    elif string.lower(os.path.split(self.PythonwinPath)[1])=="pythonwin.exe":
                        command=self.PythonwinPath+' PyTdLnc.py'
                        print"debug session..."
                        os.system(command)
                    else:
                        print "NOT VALID DEBUGGER\n Change debugger program "
            except:
                #se c'è un'eccezione a questo livello coloro di rosso il pulsante di log 
                self.wnd.launcher.log.SetBackgroundColour(wxRED)

            #Se la piattaforma è NT allora ferma il thread 
            if os.environ.has_key('OS'):
                self.MngThr1.wait()
        else:
            #Se non c'è nessun TC del HIL allora colora di grigio i led relativi e setta lo stopflgHIL a 1  
            while (os.path.exists('led.flg')):
                pass
            f=open('led_status.txt','r+')
            tmpList=f.readlines()
            exec(tmpList[0])
            for count in range(self.TD_NumTdXInstr[1]-self.TD_NumTdXInstr[0]):
                LED_STATUS.append([])
            tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
            tmpList[2]='stopflgHIL=1\n'
            f.seek(0)
            f.writelines(tmpList)
            f.close()
            f=open('led.flg','w+');f.close()



    #---------------------------------LANCIO I TEST PER NSI ((solo se almeno 1 TC è selezionato) 
        self.MngThr1.clear()
        if InstrExec[1]==1:
            try:
                #modalità RUN
                if not(self.wnd.launcher.run.GetSelection()):
                    RUN_NSI(self)
            #modalità DEBUG    
                else:
                    DEBUG_NSI(self)
            finally:
                #Se la piattaforma è NT allora ferma il thread
                if os.environ.has_key('OS'):
                    self.MngThr1.wait()
        else:
            while (os.path.exists('led.flg')):
                pass
            f=open('led_status.txt','r+')
            tmpList=f.readlines()
            exec(tmpList[0])
            for count in range(self.TD_NumTdXInstr[2]-self.TD_NumTdXInstr[1]):
                LED_STATUS.append([])
            tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
            tmpList[3]='stopflgNSI=1\n'
            f.seek(0)
            f.writelines(tmpList)
            f.close()
            f=open('led.flg','w+');f.close()

    #---------------------------------LANCIO I TEST PER LO SPIL ((solo se almeno 1 TC è selezionato) 
        self.MngThr1.clear()
        if InstrExec[2]==1:
            try:
                #modalità RUN
                if not(self.wnd.launcher.run.GetSelection()):
                    RUN_SPIL(self)
            #modalità DEBUG    
                else:
                    DEBUG_SPIL(self)
            finally:                
                #Se la piattaforma è NT allora ferma il thread
                if os.environ.has_key('OS'):
                    self.MngThr1.wait()
                    
                    
        else:
            while (os.path.exists('led.flg')):
                pass
            f=open('led_status.txt','r+')
            tmpList=f.readlines()
            exec(tmpList[0])
            for count in range(self.TD_NumTdXInstr[3]-self.TD_NumTdXInstr[2]):
                LED_STATUS.append([])
            tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
            tmpList[4]='stopflgSPIL=1\n' #Se non sono stati settati test x SPIL metto il flag a 1
            f.seek(0)
            f.writelines(tmpList)
            f.close()
            f=open('led.flg','w+');f.close()


    #----------------------------------------------------------------------------------------------------------
    #...
    #...
    #...


        #Chiudo il file di log 
        filelog=open(self.LauncherPath+'\\log.txt','a+')
        filelog.write("\nSTOP TESTS SESSION:%s"%time.ctime(time.time()))
        filelog.close()



############################################################################################################
# FUNCTION NAME : RUN_NSI
# FUNCTION DESCRIPTION: Lancia i TD per NSI, per ogni TD:
#                       - crea compile.bat e lo lancia
#                       - se compilazione Ok-->crea run.bat e lo lancia
#                       - aggiorna il file led_status.txt
#                       I parametri dei Tc selezionati, del tipo di ecu e del path sono scritti nel file
#                       command.txt dopo la compilazione nella cartella del TD 
#                       Il risultato dei Tc è passato nel file result.txt creato nella cartella del TD 
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def RUN_NSI(self):
    filelog=open('log.txt','a+')
    exec(self.EcuType)  #EcuType='sviluppo'
    NsiTestList=self.GlobalTestList[self.TD_NumTdXInstr[1]:self.TD_NumTdXInstr[2]] 
    #es: HilTestList=[[2, 2, 2, 0, 2, 1, 2, 2], [1, 2, 2, 2, 2, 1, 2, 2]]
    NsiTD_DATA=self.TD_DATA[self.TD_NumTdXInstr[1]:self.TD_NumTdXInstr[2]]
    #es: NsiTD_DATA=[['M:\\Work\\5sf.01\\Macro_Funzione2.0\\TD_Scot.6.5', 'td_glam', ['TD GLAM', 'Lambda OP', 'CA', 'CCGND', 'CCVCC', 'CCVBAT', 'plausibilit\xe0', 'riscaldatori', 'CC GNDriscaldatore']],
    #                ['M:\\Work\\5sf.01\\Macro_Funzione2.0\\TD_Scot.6.6', 'td_glambis', ['TD GLAMbis', 'Lambda OP', 'CA', 'CCGND', 'CCVCC', 'CCVBAT', 'plausibilit\xe0', 'riscaldatori', 'CC GNDriscaldatore']]]
    #eseguo i TD selezionati
    print '\n\n\n\n\n\n\nSTART NSI TESTS SESSION'
    tmpLed=[]#inizializzo la variabile temporanea che raccoglie lo stato dei led
    for indexTD in range(len(NsiTestList)):
        TypeTestListTD=NsiTestList[indexTD]
        TD_DATA=NsiTD_DATA[indexTD]
        try:
            if (TypeTestListTD.count(1) or TypeTestListTD.count(2)): #non eseguo se tutti i TC sono NO
                path=TD_DATA[0] #es: path='M:\Work\5sf.01\Macro_Funzione2.0\TD_Scot.6.5'
                #se per qualsiasi motivo alla partenza si ha il file result.txt f=open(str(path)+r'\result.txt','r+')
                if (os.path.exists(str(path)+r'\result.txt')):
                    os.remove(str(path)+r'\result.txt')
                #Creo il compile.bat
                MSDevDir="set MSDevDir="+self.NSIMSDevDir
                MSVCDir="\n\nset MSVCDir="+self.NSIMSVCDir
                NSIDir="\n\nset NSIDir="+self.NSIDir
                PATH="\n\nset PATH=%PATH%;%MSDevDir%\BIN;%MSVCDir%\BIN;"
                #includo le librerie presenti nel progetto
                IncludeLib='\n\nset INCLUDE=%INCLUDE%;%MSVCDir%\INCLUDE;%MSVCDir%\MFC\INCLUDE;%MSVCDir%\ATL\INCLUDE;%NSIDir%\INC;'
                SetLib='\n\nset LIB=%LIB%;%MSVCDir%\LIB;%MSVCDir%\MFC\LIB;%NSIDir%\LIB;'
                for PathLib in glob.glob(self.CurrentPylibPath):
                    IncludeLib=IncludeLib+str(PathLib)+';'
                    SetLib=SetLib+str(PathLib)+';'
                #es: set INCLUDE=M:\Work\NSI_00\Lib\TESTLAUNCHERVS20_BOL;M:\Work\NSI_00\Lib\modello;M:\Work\NSI_00\Lib\LibFirstIni;

                #mi metto nella directory della TD
                CDTD='\n\ncd '+str(path)
                #es: cd M:\Work\NSI_00\Macro_FunzioneNSI1\NomeTD1.5

                #mi posiziono sul disco della TD                
                Drive='\n\n'+os.path.splitdrive(str(path))[0]
                #es: M:
                
                #lancio la compilazione fornendo in uscita nel file make.log l'esito della compilazione
                command='\n\nmsdev '+ TD_DATA[1]+'.dsp /MAKE '+'"'+TD_DATA[1]+' - Win32 Debug"'+' /USEENV /OUT make.log'
                #es: msdev Id_TxMsg.dsp /MAKE "Id_TxMsg - Win32 Debug" /USEENV /OUT make.log

                #scrivo il .bat e lo eseguo                
                filebat=open('compile.bat','w+')
                filebat.write(MSDevDir)
                filebat.write(MSVCDir)
                filebat.write(NSIDir)
                filebat.write(PATH)
                filebat.write(IncludeLib)
                filebat.write(SetLib)
                filebat.write(CDTD)
                filebat.write(Drive)
                filebat.write(command)
                filebat.close()
                #Lancio la compilazione
                str2exe=str(self.LauncherPath)+'\compile'
                #se per qualsiasi motivo alla partenza si ha il file result.txt f=open(str(path)+r'\result.txt','r+')
                if (os.path.exists(str(path)+r'\make.log')):
                    os.remove(str(path)+r'\make.log')
                os.system(str2exe)   #1 se KO
                if (os.path.exists(str(path)+r'\make.log')):
                    fileerror=open(str(path)+'\make.log','r')
                    testo=fileerror.read()
                    errorstr=testo.splitlines()[-1]
                    fileerror.close()
                    error=errorstr[string.find(errorstr,'error(s)',0)-2]
                    #In caso di esito positivo creo il file command.txt in cui passo alcuni parametri utili per il test
                    if int(error)==0:
                        commandfile=open(str(path)+'\command.txt','w+')
                        commandfile.write(str(TypeTestListTD))  #test case selezionati es: [0, 0, 0, 2, 0, 0, 0, 0, 0, 0]
                        commandfile.write('\n'+EcuType) #tipo di ecu es: sviluppo
                        commandfile.write('\n'+path) #path della TD stessa es: M:\Work\NSI_00\Macro_FunzioneNSI1\NomeTD1.5
                        commandfile.write('\n'+self.LauncherPath) #path del launcher
                        commandfile.close()
                        #sempre in caso di compilazione positiva creo il .bat per il lancio del .exe
                        #vado a cercare dov'è il file first.ini tra le librerie  
                        firstiniDIR=glob.glob(self.CurrentPylibPath+r'\first.ini')
                        # In base a al tipo di ECU passo un second.ini diverso
                        if EcuType=='sviluppo': 
                            secondiniDIR=glob.glob(self.CurrentPylibPath+r'\second_DEV.ini')
                        else:   #se è di produzione 
                            secondiniDIR=glob.glob(self.CurrentPylibPath+r'\second_PROD.ini')
                        #es:
                        command='\n\n'+str(path)+'\\Debug\\'+ TD_DATA[1]+'.exe '+'"'+firstiniDIR[0]+'"'+' 0x50 '+'"'+secondiniDIR[0]+'"'
                        filebat=open('run.bat','w+')
                        filebat.write(CDTD)
                        filebat.write(Drive)
                        #Aggiunta nella variabile d'ambiente LIB i path delle librerie del progetto
                        filebat.write(SetLib)
                        filebat.write(command)
                        filebat.close()
                        #lancio il .bat
                        str2exe=str(self.LauncherPath)+r'\run'
                        os.system(str2exe)

                        #Quando il test è finito leggo lo stato dei led nel file result.txt con formato es: [0,1,1,3,2,1] 
                        f=open(str(path)+r'\result.txt','r+')
                        TDresult=f.readline()
                        str2exec='tmpLed.append('+TDresult+')'
                        f.close()
                        exec(str2exec)
                      
                        #Verifico anche che i test non abbiano avuto problemi nell'esecuzione.
                        
                        if map(CompList,TypeTestListTD,TDresult).count(0)>0:
                            filelog.write("\n    TEST %s FALLITO\n"%(TD_DATA[2][0]))
                        else:
                            filelog.write("\n    TEST %s ESEGUITO\n"%(TD_DATA[2][0]))
                        #Coloro i led della grafica
                        while (os.path.exists('led.flg')):
                            pass
                        f=open('led_status.txt','r+')
                        tmpList=f.readlines() #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
                        exec(tmpList[0])
                        LED_STATUS=LED_STATUS+tmpLed
                        tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' 
                        f.seek(0)
                        f.writelines(tmpList)
                        f.close()
                        #una volta scritto l'esito dei TC nel LED_STATUS resetto la var temporanea
                        tmpLed=[]
                        f=open('led.flg','w+');f.close()
                    else:
                        filelog.write("\n**********\nTD %s FALLITA: compilazione fallita\n %s"%(TD_DATA[2][0],testo))
                        filelog.write("\n**********\n")
##                        self.wnd.launcher.log.SetBackgroundColour(wxRED)
                        tmpLed.append([])
                else:
                    filelog.write("\n**********\nTD %s FALLITA: compilazione non possibile\n Controllare l'installazione NSI"%(TD_DATA[2][0]))
                    filelog.write("\n**********\n")
##                    self.wnd.launcher.log.SetBackgroundColour(wxRED)
                    tmpLed.append([])
            else:
                tmpLed.append([])
                
        except:
            tmpLed.append([])
            (ErrorType,ErrorValue,ErrorTB)=sys.exc_info()
            filelog.write("\n**********\nTD %s FALLITA:\nTIPO:%s\nTRACE:\n"%(TD_DATA[2][0],ErrorValue))
            traceback.print_exc(None,filelog)
            filelog.write("**********\n")
            filelog.close()
            print ("TD %s FALLITA"%TD_DATA[2][0])
##            self.wnd.launcher.log.SetBackgroundColour(wxRED)
    #Finita la sessione di test per questo strumento setto a 1 i flag di stopInstr e stopflgNSI
    while (os.path.exists('led.flg')):
        pass
    f=open('led_status.txt','r+')
    tmpList=f.readlines()
    exec(tmpList[0])
    LED_STATUS=LED_STATUS+tmpLed
    tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
    tmpList[1]='stopInstr=1\n'
    tmpList[3]='stopflgNSI=1\n'
    f.seek(0)
    f.writelines(tmpList)
    f.close()
    f=open('led.flg','w+');f.close()
    filelog.close()

############################################################################################################
# FUNCTION NAME : DEBUG_NSI
# FUNCTION DESCRIPTION: Lancia il primo TD selezionato per NSI in modalità debug  
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente
#
############################################################################################################
def DEBUG_NSI(self):
    exec(self.EcuType)  #EcuType='sviluppo'
    NsiTestList=self.GlobalTestList[self.TD_NumTdXInstr[1]:self.TD_NumTdXInstr[2]]
    NsiTD_DATA=self.TD_DATA[self.TD_NumTdXInstr[1]:self.TD_NumTdXInstr[2]]
    try:
        print"\n\n\n\n\n\n\nNSI debug session..."
        if len(NsiTestList)!=0:
            #vado in debug sul primo TD selezionato
            for indexTD in range(len(NsiTestList)):
                if (NsiTestList[indexTD].count(1) or NsiTestList[indexTD].count(2)): #non considero se tutti i TC sono NO
                    break
            TypeTestListTD=NsiTestList[indexTD]
            TD_DATA=NsiTD_DATA[indexTD]
            if (TypeTestListTD.count(1) or TypeTestListTD.count(2)): #non eseguo se tutti i TC sono NO
                path=TD_DATA[0] #es: path='M:\Work\5sf.01\Macro_Funzione2.0\TD_Scot.6.5'

                #Creo il compile.bat
                MSDevDir="set MSDevDir="+self.NSIMSDevDir
                MSVCDir="\n\nset MSVCDir="+self.NSIMSVCDir
                NSIDir="\n\nset NSIDir="+self.NSIDir
                PATH="\n\nset PATH=%PATH%;%MSDevDir%\BIN;%MSVCDir%\BIN;"
                #includo le librerie presenti nel progetto
                IncludeLib='\n\nset INCLUDE=%INCLUDE%;%MSVCDir%\INCLUDE;%MSVCDir%\MFC\INCLUDE;%MSVCDir%\ATL\INCLUDE;%NSIDir%\INC;'
                SetLib='\n\nset LIB=%LIB%;%MSVCDir%\LIB;%MSVCDir%\MFC\LIB;%NSIDir%\LIB;'
                for PathLib in glob.glob(self.CurrentPylibPath):
                    IncludeLib=IncludeLib+str(PathLib)+';'
                    SetLib=SetLib+str(PathLib)+';'
                CDTD='\n\ncd '+str(path)
                command='\n\nmsdev '+ TD_DATA[1]+'.dsw /USEENV'
                Drive='\n\n'+os.path.splitdrive(str(path))[0]
                commandfile=open(str(path)+'\command.txt','w+')
                commandfile.write(str(TypeTestListTD))
                commandfile.write('\n'+EcuType)
                commandfile.write('\n'+path)
                commandfile.write('\n'+self.LauncherPath) #path del launcher
                commandfile.close()
                filebat=open('run.bat','w+')
                filebat.write(MSDevDir)
                filebat.write(MSVCDir)
                filebat.write(NSIDir)
                filebat.write(PATH)
                filebat.write(IncludeLib)
                filebat.write(SetLib)
                filebat.write(CDTD)         #es: cd M:\Work\NSI_00\Macro_FunzioneNSI1\NomeTD1.5
                filebat.write(Drive)        #es: M:
                filebat.write(command)      #es: msdev Id_TxMsg.dsw /USEENV
                filebat.close()                    
                str2exe=str(self.LauncherPath)+r'\run'
                if os.system(str2exe):
                    filelog=open('log.txt','a+')
                    filelog.write("\n**********\nTD %s FALLITA: debug non possibile,\n Controllare l'installazione NSI"%(TD_DATA[2][0]))
                    filelog.write("\n**********\n")
                    filelog.close()
##                    self.wnd.launcher.log.SetBackgroundColour(wxRED)

    except:
        (ErrorType,ErrorValue,ErrorTB)=sys.exc_info()
        filelog.write("\n**********\nTD %s FALLITA:\nTIPO:%s\nTRACE:\n"%(TD_DATA[2][0],ErrorValue))
        traceback.print_exc(None,filelog)
        filelog.write("**********\n")
        filelog.close()
        print ("TD %s FALLITA"%TD_DATA[2][0])
##        self.wnd.launcher.log.SetBackgroundColour(wxRED)

    while (os.path.exists('led.flg')):
        pass
    f=open('led_status.txt','r+')
    tmpList=f.readlines()
    tmpList[1]='stopInstr=1\n'
    tmpList[2]='stopflgHIL=1\n'
    tmpList[3]='stopflgNSI=1\n'
    tmpList[4]='stopflgSPIL=1\n'
    f.seek(0)
    f.writelines(tmpList)
    f.close()
    f=open('led.flg','w+');f.close()


############################################################################################################
# FUNCTION NAME : RUN_SPIL
# FUNCTION DESCRIPTION: Lancia i TD per SPIL, per ogni TD:
#                       - xxxcrea compile.bat e lo lancia
#                       - xxxse compilazione Ok-->crea run.bat e lo lancia
#                       - xxxxaggiorna il file led_status.txt
#                       xxxI parametri dei Tc selezionati, del tipo di ecu e del path sono scritti nel file
#                       xxxcommand.txt dopo la compilazione nella cartella del TD 
#                       xxxIl risultato dei Tc è passato nel file result.txt creato nella cartella del TD 
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambienter
#
############################################################################################################
def RUN_SPIL(self):
    
    filelog=open('log.txt','a+')
    filelog.close()
    try:
        tmpLed=[]#inizializzo la variabile temporanea che raccoglie lo stato dei led
        exec(self.EcuType)  #EcuType='sviluppo'
        #DIrectory di installazione dell'ISS
        StartUpIssPath=os.path.dirname(self.IssInstall)
        SpilTestList=self.GlobalTestList[self.TD_NumTdXInstr[2]:self.TD_NumTdXInstr[3]] 
        #es: SpilTestList=[[2, 2, 2, 0, 2, 1, 2, 2], [1, 2, 2, 2, 2, 1, 2, 2]]
        SpilTD_DATA=self.TD_DATA[self.TD_NumTdXInstr[2]:self.TD_NumTdXInstr[3]]
        #Inizializzazione: utile solo per la exception in caso di errore prima di entrare nel for dei TD
        TD_DATA=SpilTD_DATA[0]
        #es: SpilTD_DATA=[['M:\\Work\\5sf.01\\Macro_Funzione2.0\\TD_Scot.6.5', 'td_glam', ['TD GLAM', 'Lambda OP', 'CA', 'CCGND', 'CCVCC', 'CCVBAT', 'plausibilit\xe0', 'riscaldatori', 'CC GNDriscaldatore']],
        #                ['M:\\Work\\5sf.01\\Macro_Funzione2.0\\TD_Scot.6.6', 'td_glambis', ['TD GLAMbis', 'Lambda OP', 'CA', 'CCGND', 'CCVCC', 'CCVBAT', 'plausibilit\xe0', 'riscaldatori', 'CC GNDriscaldatore']]]
        #eseguo i TD selezionati
        print '\n\n\n\n\n\n\nSTART SPIL TESTS SESSION'

        #Definizioni generali per la costruzione del file MatStrtp.m che lancerà la pre- elaborazione    
        #Ricavo il path del DD dal file load.cmm presente in M:\work\load.cmm, es:DD='Z:\\cdm\\meb\\4TV.dd'
        f=open(os.path.split(self.ProjPath)[0]+"\\load.cmm",'r+')
        testo=f.read()
        f.close()
        DD_start=string.find(testo,"DD='")
        if DD_start>=0 :
            DD_stop=string.find(testo,"\n",DD_start)
            line=testo[DD_start:DD_stop]
            exec(line)
        #Ricavo il path del DD es:ProjectSyncPath='Z:\\'
        Prj_start=string.find(testo,"ProjectSyncPath='")
        if Prj_start>=0 :
            Prj_stop=string.find(testo,"\n",Prj_start)
            line=testo[Prj_start:Prj_stop]
            exec(line)
        #Mi serve la data dell'ultima modifica del DD in quanto
        #se il .dd a cui ha puntato il DataDict.mat presente è cambiato allora devo ricreare il DataDict.mate
        DDLastChangeInSec=str(os.stat(DD)[stat.ST_MTIME])   


        #SpilMatLib='M:\\Work\\4TV_SPIL_00\\LIB\\SPILMATLIB\n'
        SpilMatLib=os.path.dirname(glob.glob(self.CurrentPrj+'\\Lib\\*\\SetCalib.m')[0]) #OK generale

        DataDictPath=self.LauncherPath+'\\SpilTmp\\DD\\DataDict.mat' #con ricerca se il load.cmm è cambiato     

        CalibPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\calibfile.txt' #OK general
        InputPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\varfile.txt' #OK general
        EventPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\eventfile.txt' #OK general
        EventVectorPath=self.LauncherPath+'\\SpilTmp\\Result\\EvtVectAll' #OK general
        OutputNamePathSpil=self.LauncherPath+'\\SpilTmp\\Result\\outname.txt'    #OK general
        PowerOnPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\PowerOn.txt'    #OK general
        #IRPath='M:\\Work\\4TV_SPIL_00\\LIB\\SPIL4TVST280\\IRlist' #ricerca del file IRlist
        if glob.glob(self.CurrentPrj+'\\Lib\\*\\IRlist.m')!=[]:
            IRPath=os.path.splitext(glob.glob(self.CurrentPrj+'\\Lib\\*\\IRlist.m')[0])[0] #OK generale
        else:
            IRPath=''
        IRlistCkdPath=self.LauncherPath+'\\SpilTmp\\Result\\IRlistCkd'   #OK generale        
            
        #OutputPathSpil='M:\\SPILLAUNCHER\\SpilTmp\\Result\\output.txt'
        OutputPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\output.txt'
        #Dir2Zip="M:\\SPILLAUNCHER\\SpilTmp"
        Dir2Zip=self.LauncherPath+'\\SpilTmp'


        #Lista della START di ISS
        #Rinomino l'eventuale T32.cmm originale in T32orig.cmm
        if os.path.exists(StartUpIssPath+'\\T32.cmm'):
            if not(os.path.exists(StartUpIssPath+'\\T32orig.cmm')):
                shutil.copy(StartUpIssPath+'\\T32.cmm',StartUpIssPath+'\\T32orig.cmm')
        StartUpIssFile=['do '+self.LauncherPath+'\\SpilTmp\\Result\\IssStrtp.cmm\n']
        f=open(StartUpIssPath+'\\T32.cmm','w+')
        f.writelines(StartUpIssFile)
        f.close()

        #ProjectPath="C:\\SPIL\\WORK\\SET4TV,02_24"
        ProjectLibPath=os.path.dirname(glob.glob(self.CurrentPrj+'\\Lib\\*\\Init.cmm')[0])
        #LibPath="C:\\SPIL\\WORK\\PROGETTO_4TV\\LIB\\SPILSCRIPT"
        LibPath=os.path.dirname(glob.glob(self.CurrentPrj+'\\Lib\\*\\SPIL.cmm')[0])
        #Leggo il settaggio del coverage
        CovFlg=str(self.wnd.Develop.Spil.CovBox.GetValue())

        for indexTD in range(len(SpilTestList)):
            TypeTestListTD=SpilTestList[indexTD]
            TD_DATA=SpilTD_DATA[indexTD]
            #Inizializzo il LED_STATUS
            TdLed=[]
            f=open('led_status.txt','r+')
            tmpList=f.readlines() #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
            exec(tmpList[0])
            LED_STATUS.append(TdLed)
            tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' 
            f.seek(0)
            f.writelines(tmpList)
            f.close()

            if (TypeTestListTD.count(1) or TypeTestListTD.count(2)): #non eseguo se tutti i TC sono NO
                TimeStartTest       = time.time()        #Tempo inizio test
                path=TD_DATA[0] #es: path='M:\Work\5sf.01\Macro_Funzione2.0\TD_Scot.6.5'
                    
                #Definizioni particolari del TD per la costruzione del file MatStrtp.m che lancerà la pre- elaborazione
                #CalibGenPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_CAL_GEN.mat' 
                CalibGenPathSdt=path+'\\'+TD_DATA[2][0]+'_CAL_GEN.mat'#OK td
                #TollPath='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\TOLL_SM06_GEN.m' #da script
                TollPath=path+'\\TOLL_'+TD_DATA[2][0]+'_GEN.m'#OK tc
                #ReportPath='M:\\WORK\\REPORTS\\report.txt'
                #ExtName è l'estensione da aggiungere alla cartella  del report per  individuare il gg hh mm ss del test 
                ExtName="("+str(time.localtime()[2])+"gg)("+str(time.localtime()[3])+"hh)("+str(time.localtime()[4])+"mm)("+str(time.localtime()[5])+"ss)"
                ReportPath=self.ReportSpil+'\\'+TD_DATA[2][0]+ExtName+'\\report.txt'
                #Apro il file di report
                os.makedirs(os.path.dirname(ReportPath))
                f=open(ReportPath,'w')
                f.close()

                PrimoTc=1
                for nTC in range(len(TypeTestListTD)):
                    if TypeTestListTD[nTC]!=0:
                        #Se esiste il file result.txt allora lo cancello
                        if os.path.exists(self.LauncherPath+'\\result.txt'):
                            os.remove(self.LauncherPath+'\\result.txt')
                        #FILE IssStrtp
                        SaveCov=''
                        LoadCov=''
                        CovView=''
                        #se è abilitato il flag del coverage salvo il contenuto del cov nella cartella del report
                        #e a partire dal 2° TC lanciato carico il TC precedente 
                        if CovFlg=="1":
                            SaveCov='a.coverage.save '+self.ReportSpil+'\\'+TD_DATA[2][0]+ExtName+'\\cov'
                            CovView='a.coverage.listfunc\n'
                            if PrimoTc==0:
                                LoadCov='a.coverage.load '+self.ReportSpil+'\\'+TD_DATA[2][0]+ExtName+'\\cov\n'
                        IssStrtp=[
                         'on error goto exception\n',\
                         '&ProjectLibPath="'+ProjectLibPath+'"\n',\
                         '&LauncherPath="'+self.LauncherPath+'\\SpilTmp\\Result'+'"\n',\
                         '&LibPath="'+LibPath+'"\n',\
                         'cd &ProjectLibPath\n',\
                         '&StopDebugTime=0\n',\
                         '&CovFlg='+CovFlg+'\n',\
                         '&exec="&ProjectLibPath"+"\\t32.cmm"\n',\
                         'do &exec\n',\
                         LoadCov,\
                         CovView,\
                         'cd '+ProjectSyncPath+'\n',\
                         '&exec="'+LibPath+'\\SPIL.cmm"\n',\
                         'do &exec\n',\
                         SaveCov,\
                         '\nquit\n',\
                         'exception:\n',
                         'OPEN #10 '+self.LauncherPath+'\\result.txt /Create\n',\
                         'WRITE #10 %CONTINUE "0"\n',\
                         'CLOSE #10\n',\
                         'quit\n']

                        #individuo il nome
                        TCname=TD_DATA[2][nTC+1]
                        indexTC=TCname[string.find(TCname,"_TP_")+4:string.find(TCname,"_TP_")+6]
                        #Solo se è il primo Tc devo aggiungere 3 righe allo MatStop.m
                        if PrimoTc:
                            MatStop2=["    IssPath='"+StartUpIssPath+"';\n",\
                                     '    GeneralReport(LauncherPath,IssPath,ReportPath)\n']
                            PrimoTc=0
                        else:
                            MatStop2=[]

                        #CalibTpPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_CAL_TP_01.mat'
                        CalibTpPathSdt=path+'\\'+TD_DATA[2][0]+'_CAL_TP_'+indexTC+'.mat'#OK tc
                        if not(os.path.exists(CalibTpPathSdt)):
                            CalibTpPathSdt=''

                        #InputPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_TP_02_in.mat'
                        InputPathSdt=path+'\\'+TD_DATA[2][0]+'_TP_'+indexTC+'_in.mat'#OK tc
                        
                        #OutputPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_TP_02_ou.mat'
                        OutputPathSdt=path+'\\'+TD_DATA[2][0]+'_TP_'+indexTC+'_ou.mat'#OK tc

                        #TestpointPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_TP_02_tx.mat' #da script
                        TestpointPathSdt=path+'\\'+TD_DATA[2][0]+'_TP_'+indexTC+'_tx.mat'#OK tc
                        if not(os.path.exists(TestpointPathSdt)):
                            TestpointPathSdt=''

                        #Dove andare a salvare lo zip dei risultati
                        ZipNamePath=os.path.dirname(ReportPath)+'\\'+TD_DATA[2][0]+'_TP'+indexTC+'.zip'
                        #Dove andare a salvare le figure dei risultati
                        FigPath=os.path.dirname(ReportPath)+'\\TP_'+indexTC;
                        #Tipo di Test 1-->OL,2-->CL
                        if TypeTestListTD[nTC]==2:
                            TIPOTEST='CL'
                        else:
                            TIPOTEST='OL'
                        #Creazione della lista che poi sarà stampata 
                        MatStrtp=["DD='"+DD+"';\n",\
                         "DataDictPath='"+DataDictPath+"';\n",\
                         "CalibGenPathSdt='"+CalibGenPathSdt+"';\n",\
                         "CalibTpPathSdt='"+CalibTpPathSdt+"';\n",\
                         "CalibPathSpil='"+CalibPathSpil+"';\n",\
                         "InputPathSdt='"+InputPathSdt+"';\n",\
                         "InputPathSpil='"+InputPathSpil+"';\n",\
                         "EventPathSpil='"+EventPathSpil+"';\n",\
                         "EventVectorPath='"+EventVectorPath+"';\n",\
                         "OutputPathSdt='"+OutputPathSdt+"';\n",\
                         "TestpointPathSdt='"+TestpointPathSdt+"';\n",\
                         "OutputNamePathSpil='"+OutputNamePathSpil+"';\n",\
                         "IRPath='"+IRPath+"';\n",\
                         "IRlistCkdPath='"+IRlistCkdPath+"';\n",\
                         "TollPath='"+TollPath+"';\n",\
                         "PowerOnPathSpil='"+PowerOnPathSpil+"';\n",\
                         "ProjectLibPath='"+ProjectLibPath+"';\n",\
                         "LauncherPath='"+self.LauncherPath+"';\n",\
                         "DDTime="+DDLastChangeInSec+";\n",\
                         "IOCerr='';\n",\
                         "try\n",\
                         '    cd '+SpilMatLib+'\n',\
                         '    fid=fopen(DataDictPath);\n','    if fid==-1\n','        DDparser(DD,DataDictPath,DDTime);\n','    else\n',"        load(DataDictPath,'DDGenTime');\n",'        if DDGenTime==DDTime\n',"            fprintf('\\nUtilizzo il Data Dictionary già esistente DataDict.mat');\n",'        else\n',"            fprintf('\\nCreo DataDict.mat nuovo perchè l''esistente non è aggiornato...');\n",'            DDparser(DD,DataDictPath,DDTime);\n','        end\n','    end\n',\
                         '    IOCerr=IOCdiag(DataDictPath,CalibGenPathSdt,CalibTpPathSdt,InputPathSdt,OutputPathSdt,TestpointPathSdt,TollPath);\n','    if ~isempty(IOCerr)\n',"        error('    SDT NON VALIDA: ');\n","    end\n",\
                         '    SetCalib(CalibGenPathSdt,CalibTpPathSdt,CalibPathSpil,DataDictPath,IRPath,IRlistCkdPath);\n',\
                         '    SetVar(InputPathSdt,InputPathSpil,DataDictPath,EventPathSpil,EventVectorPath,IRlistCkdPath,PowerOnPathSpil,ProjectLibPath);\n',\
                         '    NameOut(OutputNamePathSpil,TollPath,DataDictPath);\n',\
                         "catch\n",\
                         "    ResPath=[LauncherPath '\\result.txt'];\n",\
                         "    fid = fopen(ResPath,'w+');\n",\
                         "    fprintf(fid,'0');\n",\
                         "    fclose(fid);\n\n",\
                         "    LogPath=[LauncherPath '\log.txt'];\n",\
                         "    ErrorLog=['    ' lasterr];\n",\
                         "    ErrorLog = strrep(ErrorLog,'\\','\\\\');\n",\
                         "    fid = fopen(LogPath,'a');\n",\
                         "    fprintf(fid,'\\n    Pre-processing error:\\n');\n",\
                         "    fprintf(fid,ErrorLog);\n",\
                         "    fprintf(fid,IOCerr);\n",\
                         "    fclose(fid);\n",\
                         "end\n",\
                         'clear all\n', "fprintf('\\n\\nFINE PRE-ELABORAZIONE');\n", 'quit\n']

                        MatStop1=["OutputPathSdt='"+OutputPathSdt+"';\n",\
                                 "TestpointPathSdt='"+TestpointPathSdt+"';\n",\
                                 "DataDictPath='"+DataDictPath+"';\n",\
                                 "OutputPathSpil='"+OutputPathSpil+"';\n",\
                                 "EventVectorPath='"+EventVectorPath+"';\n",\
                                 "TollPath='"+TollPath+"';\n",\
                                 "ReportPath='"+ReportPath+"';\n",\
                                 "ZipNamePath='"+ZipNamePath+"';\n",\
                                 "Dir2Zip='"+Dir2Zip+"';\n",\
                                 "FigPath='"+FigPath+"';\n",\
                                 "TIPOTEST='"+TIPOTEST+"';\n",\
                                 "ResPath='"+self.LauncherPath+"\\result.txt';\n",\
                                 "LauncherPath='"+self.LauncherPath+"';\n",\
                                 "try\n",\
                                 '    cd '+SpilMatLib+'\n']
                        
                        MatStop3=[    'FilterOut(OutputPathSdt,TestpointPathSdt,OutputPathSpil,DataDictPath,EventVectorPath,TollPath,ReportPath,ZipNamePath,Dir2Zip,FigPath,ResPath,TIPOTEST);\n',\
                                 "    fprintf('\\n\\nFINE POST-ELABORAZIONE');\n",\
                                 "catch\n",\
                                 "    ResPath=[LauncherPath '\\result.txt'];\n",\
                                 "    fid = fopen(ResPath,'w+');\n",\
                                 "    fprintf(fid,'0');\n",\
                                 "    fclose(fid);\n\n",\
                                 "    LogPath=[LauncherPath '\log.txt'];\n",\
                                 "    ErrorLog=['    ' lasterr];\n",\
                                 "    ErrorLog = strrep(ErrorLog,'\\','\\\\');\n",\
                                 "    fid = fopen(LogPath,'a');\n",\
                                 "    fprintf(fid,'\\n    Post-processing error:\\n');\n",\
                                 "    fprintf(fid,ErrorLog);\n",\
                                 "    fclose(fid);\n",\
                                 "end\n",\
                                 'quit\n']
                        MatStop=MatStop1+MatStop2+MatStop3

                        
                        #Se esistente ripulisco la cartella temporanea SpilTmp, altrimento la creo nuova
                        if os.path.exists(self.LauncherPath+'\\SpilTmp\\Result'):
                            for file in glob.glob(self.LauncherPath+'\\SpilTmp\\Result\\*.*'):
                                os.remove(file)
                        else:
                            os.makedirs(self.LauncherPath+'\\SpilTmp\\Result')

                        #Se non esistente creo nuova cartella DD
                        if not(os.path.exists(self.LauncherPath+'\\SpilTmp\\DD')):
                            os.makedirs(self.LauncherPath+'\\SpilTmp\\DD')


                        #Riga di lancio del MatStrtp.m
                        StartUpFile=" /r cd('"+self.LauncherPath+"\SpilTmp\Result');MatStrtp"
                        #Stampa del file MatStrtp.m
                        f=open(self.LauncherPath+'\\SpilTmp\\Result\\MatStrtp.m','w+')
                        f.writelines(MatStrtp)
                        f.close()
                        os.system(self.MtlbInstall+StartUpFile)
                        SPILcomment=""
                        # Se non c'è stato nessun problema nell'esecuzione dello  MatStrtp. continua 
                        if not(os.path.exists(self.LauncherPath+'\\result.txt')):

                            #Stampa del file IssStrtp.m
                            f=open(self.LauncherPath+'\\SpilTmp\\Result\\IssStrtp.cmm','w+')
                            f.writelines(IssStrtp)
                            f.close()
                            #Creo il RunIss.bat per lanciare l'ISS
                            CDTD='\ncd '+StartUpIssPath+'\n'+os.path.splitdrive(StartUpIssPath)[0]
                            command='\n'+self.IssInstall
                            #scrivo il .bat e lo eseguo                
                            filebat=open(self.LauncherPath+'\\RunIss.bat','w+')
                            filebat.write(CDTD)
                            filebat.write(command)
                            filebat.close()
                            os.system(self.LauncherPath+'\\RunIss.bat')
                            if not(os.path.exists(self.LauncherPath+'\\result.txt')):
                                
                                #Stampa del file startup.m
                                StartUpFile=" /r cd('"+self.LauncherPath+"\SpilTmp\Result');MatStop"
                                #Stampa del file MatStop.m
                                os.makedirs(FigPath)
                                f=open(self.LauncherPath+'\\SpilTmp\\Result\\MatStop.m','w+')
                                f.writelines(MatStop)
                                f.close()
                                #se per qualsiasi motivo alla partenza si ha il file result.txt cancellalo
                                if (os.path.exists(self.LauncherPath+r'\result.txt')):
                                    os.remove(self.LauncherPath+r'\result.txt')
                                os.system(self.MtlbInstall+StartUpFile)
                            else:
                                SPILcomment="    Errore PRACTICE, rilancia il test in modalità debug per maggiori info\n" 
                        #Quando il test è finito leggo lo stato dei led nel file result.txt con formato es: [0,1,1,3,2,1] 
                        f=open(self.LauncherPath+r'\result.txt','r+')
                        TcRes=f.readline()                        
                        f.close()

                        str2exec='TdLed.append('+TcRes+')'
                        exec(str2exec)
                        filelog=open('log.txt','a+')
                        if TcRes=='0':
                            filelog.write("\n    TEST %s FALLITO\n%s"%(TCname,SPILcomment))
                        else:
                            filelog.write("\n    TEST %s ESEGUITO\n"%(TCname))
                        filelog.close()
                        #Coloro i led della grafica
                        while (os.path.exists('led.flg')):
                            pass
                        f=open(self.LauncherPath+r'\led_status.txt','r+')
                        tmpList=f.readlines() #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
                        exec(tmpList[0])
#                        TdLed.append(2)
                        LED_STATUS[-1]=TdLed
                        tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' 
                        f.seek(0)
                        f.writelines(tmpList)
                        f.close()
                        #una volta scritto l'esito dei TC nel LED_STATUS resetto la var temporanea
                        f=open('led.flg','w+');f.close()

                        #Aggiungo nello zip del TC il coverage cumulativo raggiunto col TC stesso
                        if CovFlg=="1":
                            myzip=zipfile.ZipFile(ZipNamePath,'a')
                            myzip.write(self.ReportSpil+'\\'+TD_DATA[2][0]+ExtName+'\\cov.acd')
                            myzip.close()


                    #Apro il file di report e compilo i campi NOT_DONE per i test case non eseguiti
                    else:
                        TdLed.append(0)
                        f=open(ReportPath,'a+')
                        NotDoneStr=['$TEST_CASE_RESULT_BEGIN\n',\
                                  'NOT_DONE\n',\
                                  '$TEST_CASE_RESULT_END\n\n',\
                                  '$ATTACHED_FILE_LIST_BEGIN\n',\
                                  '$ATTACHED_FILE_LIST_END\n\n',\
                                  '$TEST_CASE_RESULT_BEGIN\n',\
                                  '$TEST_CASE_RESULT_END\n\n']
                        f.writelines(NotDoneStr)
                        f.close()
                #Aggiungo al report i tag dei METRICS
                # Calcolo tempo esecuzione test
                TimeStopTest       = time.time()
                TimeEsecTest       = '%.4f'%((TimeStopTest - TimeStartTest)/3600)
                f=open(ReportPath,'a+')
                MetricsStr=['$TEST_REPORT_METRICS_BEGIN\n',\
                          TimeEsecTest+'\n',\
                          '$TEST_REPORT_METRICS_END\n\n']
                f.writelines(MetricsStr)
                #Aggiungo al report i tag dei SETUP
                if (os.path.exists(path+'\\LibraryList.txt')):
                    f=open(path+'\\LibraryList.txt','r+')
                    LibList=f.readlines()
                    f.close()
                else:
                    LibList=['\nLibraryList.txt non trovata\n']                    
                f=open(os.path.split(self.ProjPath)[0]+"\\load.cmm",'r+')
                LoadList=f.readlines()
                f.close()
                f=open(ReportPath,'a+')
                Setup=['$TEST_SETUP_BEGIN\n']+LibList+LoadList+['\n$TEST_SETUP_END\n\n']
                f.writelines(Setup)
                f.close()
    except:
        tmpLed.append([])
        (ErrorType,ErrorValue,ErrorTB)=sys.exc_info()
        filelog=open('log.txt','a+')
        filelog.write("\n**********\nTD %s FALLITA:\nTIPO:%s\nTRACE:\n"%(TD_DATA[2][0],ErrorValue))
        traceback.print_exc(None,filelog)
        filelog.write("**********\n")
        filelog.close()
        print ("TD %s FALLITA"%TD_DATA[2][0])
        self.wnd.launcher.log.SetBackgroundColour(wxRED)

    #Finita la sessione di test per questo strumento setto a 1 i flag di stopInstr e stopflgSPIL
    while (os.path.exists('led.flg')):
        pass
    f=open('led_status.txt','r+')
    tmpList=f.readlines()
    exec(tmpList[0])
    LED_STATUS=LED_STATUS+tmpLed
    tmpList[0]='LED_STATUS='+str(LED_STATUS)+'\n' #es: LED_STATUS=[[2, 2, 3, 3, 2, 3, 2, 2],[...]]
    tmpList[1]='stopInstr=1\n'
    tmpList[4]='stopflgSPIL=1\n'
    f.seek(0)
    f.writelines(tmpList)
    f.close()
    f=open('led.flg','w+');f.close()
    filelog.close()
    #Riporto gli startup originali
    if (os.path.exists(StartUpIssPath+'\\T32orig.cmm')):
        shutil.copy(StartUpIssPath+'\\T32orig.cmm',StartUpIssPath+'\\T32.cmm')
        os.remove(StartUpIssPath+'\\T32orig.cmm')
    else:
        if (os.path.exists(StartUpIssPath+'\\T32.cmm')):
            os.remove(StartUpIssPath+'\\T32.cmm')


############################################################################################################
# FUNCTION NAME : DEBUG_SPIL
# FUNCTION DESCRIPTION: Lancia il primo TD selezionato per SPIL in modalità debug  
#
# INPUT PARAMETERS:
#           1) self: oggetto che contiene le proprietà di tutto l'ambiente 
#
############################################################################################################
def DEBUG_SPIL(self):
    filelog=open('log.txt','a+')
    try:
        SpilTestList=self.GlobalTestList[self.TD_NumTdXInstr[2]:self.TD_NumTdXInstr[3]]
        SpilTD_DATA=self.TD_DATA[self.TD_NumTdXInstr[2]:self.TD_NumTdXInstr[3]]
        #Leggo tempo settato per fermarsi e rimetto il default a 0
        Time2Stop=self.wnd.Develop.Spil.DebugTimeText.GetValue()
        self.wnd.Develop.Spil.DebugTimeText.SetValue('0')
        #Default del flag di coverage
        self.wnd.Develop.Spil.CovBox.SetValue(False)
        print"\n\n\n\n\n\n\nSpil debug session..."
        if len(SpilTestList)!=0:
            #vado in debug sul primo TD selezionato
            for indexTD in range(len(SpilTestList)):
                if (SpilTestList[indexTD].count(1) or SpilTestList[indexTD].count(2)): #non considero se tutti i TC sono NO
                    break
            TypeTestListTD=SpilTestList[indexTD]
            TD_DATA=SpilTD_DATA[indexTD]
            if (TypeTestListTD.count(1) or TypeTestListTD.count(2)): #non eseguo se tutti i TC sono NO 
                path=TD_DATA[0] #es: path='M:\Work\5sf.01\Macro_Funzione2.0\TD_Scot.6.5'

            #Ricavo il path del DD dal file load.cmm presente in M:\work\load.cmm, es:DD='Z:\\cdm\\meb\\4TV.dd'
            f=open(os.path.split(self.ProjPath)[0]+"\\load.cmm",'r+')
            testo=f.read()
            f.close()
            DD_start=string.find(testo,"DD='")
            if DD_start>=0 :
                DD_stop=string.find(testo,"\n",DD_start)
                line=testo[DD_start:DD_stop]
                exec(line)
            #Ricavo il path del DD es:ProjectSyncPath='Z:\\'
            Prj_start=string.find(testo,"ProjectSyncPath='")
            if Prj_start>=0 :
                Prj_stop=string.find(testo,"\n",Prj_start)
                line=testo[Prj_start:Prj_stop]
                exec(line)
            #Mi serve la data dell'ultima modifica del DD in quanto
            #se il .dd a cui ha puntato il DataDict.mat presente è cambiato allora devo ricreare il DataDict.mate
            DDLastChangeInSec=str(os.stat(DD)[stat.ST_MTIME])   

            tmpLed=[]#inizializzo la variabile temporanea che raccoglie lo stato dei led

            #Definizioni generali per la costruzione del file MatStrtp.m che lancerà la pre- elaborazione    

            #SpilMatLib='M:\\Work\\4TV_SPIL_00\\LIB\\SPILMATLIB\n'
            SpilMatLib=os.path.dirname(glob.glob(self.CurrentPrj+'\\Lib\\*\\SetCalib.m')[0]) #OK generale

            DataDictPath=self.LauncherPath+'\\SpilTmp\\DD\\DataDict.mat' #con ricerca se il load.cmm è cambiato     

            CalibPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\calibfile.txt' #OK general
            InputPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\varfile.txt' #OK general
            EventPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\eventfile.txt' #OK general
            EventVectorPath=self.LauncherPath+'\\SpilTmp\\Result\\EvtVectAll.mat' #OK general
            OutputNamePathSpil=self.LauncherPath+'\\SpilTmp\\Result\\outname.txt'    #OK general
            PowerOnPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\PowerOn.txt'    #OK general
            #OutputPathSpil='M:\\SPILLAUNCHER\\SpilTmp\\Result\\output.txt'
            OutputPathSpil=self.LauncherPath+'\\SpilTmp\\Result\\output.txt'
            #Dir2Zip="M:\\SPILLAUNCHER\\SpilTmp"
            Dir2Zip=self.LauncherPath+'\\SpilTmp'
            StartUpIssPath=os.path.dirname(self.IssInstall)
            #IRPath='M:\\Work\\4TV_SPIL_00\\LIB\\SPIL4TVST280\\IRlist' #ricerca del file IRlist
            if glob.glob(self.CurrentPrj+'\\Lib\\*\\IRlist.m')!=[]:
                IRPath=os.path.splitext(glob.glob(self.CurrentPrj+'\\Lib\\*\\IRlist.m')[0])[0] #OK generale
            else:
                IRPath=''
            IRlistCkdPath=self.LauncherPath+'\\SpilTmp\\Result\\IRlistCkd'   #OK generale

            #ProjectPath="C:\\SPIL\\WORK\\SET4TV,02_24"
            ProjectLibPath=os.path.dirname(glob.glob(self.CurrentPrj+'\\Lib\\*\\Init.cmm')[0])
            #LibPath="C:\\SPIL\\WORK\\PROGETTO_4TV\\LIB\\SPILSCRIPT"
            LibPath=os.path.dirname(glob.glob(self.CurrentPrj+'\\Lib\\*\\SPIL.cmm')[0])
            IssStrtp=['&ProjectLibPath="'+ProjectLibPath+'"\n',\
             '&LauncherPath="'+self.LauncherPath+'\\SpilTmp\\Result'+'"\n',\
             '&LibPath="'+LibPath+'"\n',\
             'cd &ProjectLibPath\n',\
             '&StopDebugTime='+Time2Stop+'.\n',\
             '&CovFlg=0\n',\
             '&exec="&ProjectLibPath"+"\\t32.cmm"\n',\
             'do &exec\n',\
             'cd '+ProjectSyncPath+'\n',\
             '&exec="'+LibPath+'\\SPIL.cmm"\n',\
             'do &exec\n']
            #Definizioni particolari del TD per la costruzione del file MatStrtp.m che lancerà la pre- elaborazione
            #CalibGenPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_CAL_GEN.mat' 
            CalibGenPathSdt=path+'\\'+TD_DATA[2][0]+'_CAL_GEN.mat'#OK td
            #TollPath='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\TOLL_SM06_GEN.m' #da script
            TollPath=path+'\\TOLL_'+TD_DATA[2][0]+'_GEN.m'#OK tc


            #ReportPath='M:\\WORK\\REPORTS\\SM06_16143550\\report.txt'
            #ExtName è l'estensione da aggiungere alla cartella  del report per  individuare il gg hh mm ss del test 
            ExtName="("+str(time.localtime()[2])+"gg)("+str(time.localtime()[3])+"hh)("+str(time.localtime()[4])+"mm)("+str(time.localtime()[5])+"ss)"
            ReportPath=self.ReportSpil+'\\'+TD_DATA[2][0]+ExtName+'\\report.txt'
            #Apro il file di report
            os.makedirs(os.path.dirname(ReportPath))

            #creo il file di report e appendo i tag dei SETUP
            if (os.path.exists(path+'\\LibraryList.txt')):
                f=open(path+'\\LibraryList.txt','r+')
                LibList=f.readlines()
                f.close()
            else:
                LibList=['\nLibraryList.txt non trovata\n']
            f=open(os.path.split(self.ProjPath)[0]+"\\load.cmm",'r+')
            LoadList=f.readlines()
            f.close()
            f=open(ReportPath,'w')
            Setup=['$TEST_SETUP_BEGIN\n']+LibList+LoadList+['\n$TEST_SETUP_END\n\n']
            f.writelines(Setup)
            f.close()

            for nTC in range(len(TypeTestListTD)):
                if TypeTestListTD[nTC]!=0:
                    TCname=TD_DATA[2][nTC+1]
                    indexTC=TCname[string.find(TCname,"_TP_")+4:string.find(TCname,"_TP_")+6]

                    #CalibTpPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_CAL_TP_01.mat'
                    CalibTpPathSdt=path+'\\'+TD_DATA[2][0]+'_CAL_TP_'+indexTC+'.mat'#OK tc
                    if not(os.path.exists(CalibTpPathSdt)):
                        CalibTpPathSdt=''

                    #InputPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_TP_02_in.mat'
                    InputPathSdt=path+'\\'+TD_DATA[2][0]+'_TP_'+indexTC+'_in.mat'#OK tc
                    
                    #OutputPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_TP_02_ou.mat'
                    OutputPathSdt=path+'\\'+TD_DATA[2][0]+'_TP_'+indexTC+'_ou.mat'#OK tc

                    #TestpointPathSdt='M:\\Work\\4TV_SPIL_00\\MACRO_SFTM\\TD_SM06_4TV\\SM06_TP_02_tx.mat' #da script
                    TestpointPathSdt=path+'\\'+TD_DATA[2][0]+'_TP_'+indexTC+'_tx.mat'#OK tc
                    if not(os.path.exists(TestpointPathSdt)):
                        TestpointPathSdt=''

                    #Dove andare a salvare lo zip dei risultati
                    ZipNamePath=os.path.dirname(ReportPath)+'\\'+TD_DATA[2][0]+'_TP'+indexTC+'.zip'
                    #Dove andare a salvare le figure dei risultati
                    FigPath=os.path.dirname(ReportPath)+'\\TP_'+indexTC;
                    #Tipo di Test 1-->OL,2-->CL
                    if TypeTestListTD[nTC]==2:
                        TIPOTEST='CL'
                    else:
                        TIPOTEST='OL'
                    #Creazione della lista che poi sarà stampata 
                    MatStrtp=['cd '+SpilMatLib+'\n',\
                     "DD='"+DD+"';\n",\
                     "DataDictPath='"+DataDictPath+"';\n",\
                     "CalibGenPathSdt='"+CalibGenPathSdt+"';\n",\
                     "CalibTpPathSdt='"+CalibTpPathSdt+"';\n",\
                     "CalibPathSpil='"+CalibPathSpil+"';\n",\
                     "InputPathSdt='"+InputPathSdt+"';\n",\
                     "InputPathSpil='"+InputPathSpil+"';\n",\
                     "EventPathSpil='"+EventPathSpil+"';\n",\
                     "EventVectorPath='"+EventVectorPath+"';\n",\
                     "OutputPathSdt='"+OutputPathSdt+"';\n",\
                     "TestpointPathSdt='"+TestpointPathSdt+"';\n",\
                     "OutputNamePathSpil='"+OutputNamePathSpil+"';\n",\
                     "IRPath='"+IRPath+"';\n",\
                     "IRlistCkdPath='"+IRlistCkdPath+"';\n",\
                     "TollPath='"+TollPath+"';\n",\
                     "PowerOnPathSpil='"+PowerOnPathSpil+"';\n",\
                     "ProjectLibPath='"+ProjectLibPath+"';\n",\
                     "DDTime="+DDLastChangeInSec+";\n",\
                     'fid=fopen(DataDictPath);\n','if fid==-1\n','    DDparser(DD,DataDictPath,DDTime);\n','else\n',"    load(DataDictPath,'DDGenTime');\n",'   if DDGenTime==DDTime\n',"        fprintf('\\nUtilizzo il Data Dictionary già esistente DataDict.mat');\n",'    else\n',"        fprintf('\\nCreo DataDict.mat nuovo perchè l''esistente non è aggiornato...');\n",'        DDparser(DD,DataDictPath,DDTime);\n','    end\n','end\n',\
                     'SetCalib(CalibGenPathSdt,CalibTpPathSdt,CalibPathSpil,DataDictPath,IRPath,IRlistCkdPath);\n',\
                     'SetVar(InputPathSdt,InputPathSpil,DataDictPath,EventPathSpil,EventVectorPath,IRlistCkdPath,PowerOnPathSpil,ProjectLibPath);\n',\
                     'NameOut(OutputNamePathSpil,TollPath,DataDictPath);\n',\
                     'clear all\n', "fprintf('\\n\\nFINE PRE-ELABORAZIONE');\n", '%quit\n']


                    MatStop1=['cd '+SpilMatLib+'\n',\
                             "OutputPathSdt='"+OutputPathSdt+"';\n",\
                             "TestpointPathSdt='"+TestpointPathSdt+"';\n",\
                             "DataDictPath='"+DataDictPath+"';\n",\
                             "OutputPathSpil='"+OutputPathSpil+"';\n",\
                             "EventVectorPath='"+EventVectorPath+"';\n",\
                             "TollPath='"+TollPath+"';\n",\
                             "ReportPath='"+ReportPath+"';\n",\
                             "ZipNamePath='"+ZipNamePath+"';\n",\
                             "Dir2Zip='"+Dir2Zip+"';\n",\
                             "FigPath='"+FigPath+"';\n",\
                             "TIPOTEST='"+TIPOTEST+"';\n",\
                             "ResPath='"+self.LauncherPath+"\\result.txt';\n"]

                    MatStop2=["LauncherPath='"+self.LauncherPath+"';\n",\
                             "IssPath='"+StartUpIssPath+"';\n",\
                             'GeneralReport(LauncherPath,IssPath,ReportPath)\n']

                    MatStop3=['FilterOut(OutputPathSdt,TestpointPathSdt,OutputPathSpil,DataDictPath,EventVectorPath,TollPath,ReportPath,ZipNamePath,Dir2Zip,FigPath,ResPath,TIPOTEST);\n',\
                             "fprintf('\\n\\nFINE POST-ELABORAZIONE');\n",\
                             '%quit\n']
                    MatStop=MatStop1+MatStop2+MatStop3

                    #Se esistente ripulisco la cartella temporanea SpilTmp, altrimento la creo nuova
                    if os.path.exists(self.LauncherPath+'\\SpilTmp\\Result'):
                        for file in glob.glob(self.LauncherPath+'\\SpilTmp\\Result\\*.*'):
                            os.remove(file)
                    else:
                        os.makedirs(self.LauncherPath+'\\SpilTmp\\Result')

                    #Se non esistente creo nuova cartella DD
                    if not(os.path.exists(self.LauncherPath+'\\SpilTmp\\DD')):
                        os.makedirs(self.LauncherPath+'\\SpilTmp\\DD')


                    #Stampa del file MatStrtp.m
                    f=open(self.LauncherPath+'\\SpilTmp\\Result\\MatStrtp.m','w+')
                    f.writelines(MatStrtp)
                    f.close()
                    #Stampa del file IssStrtp.m
                    f=open(self.LauncherPath+'\\SpilTmp\\Result\\IssStrtp.cmm','w+')
                    f.writelines(IssStrtp)
                    f.close()
                    #Stampa del file MatStop.m
                    os.makedirs(FigPath)
                    f=open(self.LauncherPath+'\\SpilTmp\\Result\\MatStop.m','w+')
                    f.writelines(MatStop)
                    f.close()
                    # Scrivo solo x il primo dei TC selezionati

                    break
                
    except:
        (ErrorType,ErrorValue,ErrorTB)=sys.exc_info()
        filelog.write("\n**********\nTD %s FALLITA:\nTIPO:%s\nTRACE:\n"%(TD_DATA[2][0],ErrorValue))
        traceback.print_exc(None,filelog)
        filelog.write("**********\n")
        print ("TD %s FALLITA"%TD_DATA[2][0])
        self.wnd.launcher.log.SetBackgroundColour(wxRED)
    #Finita la sessione di test per questo strumento setto a 1 i flag di stopInstr e stopflgSPIL
    while (os.path.exists('led.flg')):
        pass
    f=open('led_status.txt','r+')
    tmpList=f.readlines()
    tmpList[0]='LED_STATUS=[[]]\n' 
    tmpList[1]='stopInstr=0\n' #lascio a 0 altrimenti si colora il log di rosso
    tmpList[4]='stopflgSPIL=1\n'
    f.seek(0)
    f.writelines(tmpList)
    f.close()
    f=open('led.flg','w+');f.close()
    filelog.close()
    #Riporto gli startup originali
    if (os.path.exists(StartUpIssPath+'\\T32orig.cmm')):
        shutil.copy(StartUpIssPath+'\\T32orig.cmm',StartUpIssPath+'\\T32.cmm')
        os.remove(StartUpIssPath+'\\T32orig.cmm')
    else:
        if (os.path.exists(StartUpIssPath+'\\T32.cmm')):
            os.remove(StartUpIssPath+'\\T32.cmm')





app=App(0)
app.MainLoop()


# proc2a.py
"Un processo che genera numeri casuali e li salva nel file data.txt"

import random

def main():
    ro = random.Random()
    out = file("data.txt", "w")
    for number in ro.sample(range(1000), 100):
        print >> out, number
    out.close()
    print "Dati salvati sul file 'data.txt'"
    
if __name__ == "__main__":
    main()



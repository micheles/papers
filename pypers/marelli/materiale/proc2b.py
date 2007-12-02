# proc2b.py

"Un processo che genera l'istogramma histo.png dai dati in data.txt"

from pylab import hist,savefig

def main():
    hist([int(n) for n in file("dat.txt")], 10)
    savefig("histo.png")
    print "Istogramma salvato sul file 'histo.png'"

if __name__ == "__main__":
    main()



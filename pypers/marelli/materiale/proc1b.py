# proc1b.py
  
import time, sys

def main():
    try:
        f = file("proc1b.py")
        for i in range(10):
            print "world"
            if i == 5:
                raise RuntimeError("Ahia!")
            time.sleep(1)
    finally:
        f.close()
        print "Il file è stato chiuso correttamente."

if __name__ == "__main__":
    main()



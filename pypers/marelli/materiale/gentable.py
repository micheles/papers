# gentable.py

def gentable(N):
  for i in range(1, N+1):
      for j in range(1, N+1):
          yield i*j

def printtable(lst, N):
  for i, el in enumerate(lst):
      print "%4d" % el,
      if (i+1) % 10 == 0:
          print

if __name__ == "__main__":
  printtable(gentable(10), 10)



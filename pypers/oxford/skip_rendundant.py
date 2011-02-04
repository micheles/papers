# skip_rendundant.py

def skip_redundant(iterable, skipset=None):
   "Redundant items are repeated items or items in the original skipset."
   if skipset is None: skipset = set()
   for item in iterable:
       if item not in skipset:
           skipset.add(item)
           yield item
         


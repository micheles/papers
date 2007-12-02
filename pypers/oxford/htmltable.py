# htmltable.py

def HTMLTablegen(table):
    yield "<table>"
    for row in table:
       yield "<tr>"
       for col in row:
           yield "<td>%s</td>" % col
       yield "</tr>"
    yield "</table>"

def test():
    return "\n".join(HTMLTablegen([["Row", "City"], 
                       [1,'London'], [2, 'Oxford']]))

if __name__ == "__main__": # example
    print test()



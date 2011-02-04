def prova():
    yield "ciao"
    yield "marco"

for line in prova():
    print line

error = False
it = prova()
while not error:
    try:
        print it.next()
    except StopIteration:
        error = True

print repr("\n".join(prova()))

print str.join("\n", prova())
["A", 2].join("\n")

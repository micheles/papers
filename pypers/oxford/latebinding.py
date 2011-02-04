def toplevel():
    a = 1
    def f():
        print a
    a = 2
    f()

toplevel()

func = list(lambda : i for i in range(10))
print func[0]()

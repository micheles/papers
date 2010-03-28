result = []
for i in range(10):
    result.append(lambda : i)


# could be converted in

result = []
def for_block(i):
    result.append(lambda : i)    
for i in range(10): for_block(i)
    
#func = list(lambda : i for i in range(10))
#print func[0]()
print result[1]()

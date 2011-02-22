word = "Anime"

for n in range(3):
    spaces = " " * n 
    print spaces.join(word)

for n in range(3)[::-1]:
    spaces = " " * n 
    print spaces.join(word)

print range(3)
print range(3)[::-1]

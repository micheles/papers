import textwrap

text=file('meta.txt').read()
print textwrap.fill(text,70,replace_whitespace=False)

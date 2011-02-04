print '*'*70
print """This is frontend to the script "test.py" that will test the scripts
contained in the book "Object Oriented Programming in Python" and will
create a module called "oopp" needed to run properly the examples."""
print '*'*70

if raw_input("Do you want to continue? (y/n) ")!='y': raise SystemExit

print; execfile('test.py')

raw_input('Press Return to continue ...')

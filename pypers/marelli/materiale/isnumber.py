# isnumber.py

def is_number(arg):
    "Verifica se la stringa arg è un numero valido"
    try:
        float(arg)
    except ValueError:
        return False
    else:
        return True



import operator

def calc(op, numbers):
    """
    usage: %prog op numbers ... where op can be sum or mul
    """
    if op == 'sum':
        print sum(map(float, numbers))
    elif op == 'mul':
        print reduce(operator.__mul__, map(float, numbers), 1)
    else:
        raise ValueError('Invalid operator %r' % op)

if __name__ == '__main__':
    import clap; clap.call(calc)

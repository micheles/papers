"""
The tests are runnable with nose, with py.test, or even as standalone script
"""

import sys
import clap3

def expect_exit(func, *args, **kw):
    try:
        func(*args, **kw)
    except SystemExit:
        pass
    else:
        raise RuntimeError('SystemExit expected, got none!')

def f1(delete: "d: delete a file", args=()):
    pass

def test_p1():
    p1 = clap3.parser_from(f1)
    arg = p1.parse_args(['-d', 'foo', 'arg1', 'arg2'])
    assert arg.delete == 'foo'
    assert arg.args == ['arg1', 'arg2']

    arg = p1.parse_args([])
    assert arg.delete is None, arg.delete
    assert arg.args is (), arg.args

def f2(arg1, delete: "d: delete a file", args=()):
    pass

def test_p2():
    p2 = clap3.parser_from(f2)
    arg = p2.parse_args(['-d', 'foo', 'arg1', 'arg2'])
    assert arg.delete == 'foo', arg.delete
    assert arg.arg1 == 'arg1', arg.arg1
    assert arg.args == ['arg2'], arg.args

    arg = p2.parse_args(['arg1'])
    assert arg.delete is None, arg.delete
    assert arg.args is (), arg.args
    assert arg, arg
    
    expect_exit(p2.parse_args, [])

def f3(arg1, delete: "d: delete a file"):
    pass

def test_p3():
    p3 = clap3.parser_from(f3)
    arg = p3.parse_args(['arg1'])
    assert arg.delete is None, arg.delete
    assert arg.arg1 == 'arg1', arg.args

    expect_exit(p3.parse_args, ['arg1', 'arg2'])
    expect_exit(p3.parse_args, [])

def f4(delete: "d: delete a file",
       delete_all: "a, delete all files",
       color: "c: set default color"="black"):
    pass

def test_p4():
    p4 = clap3.parser_from(f4)
    arg = p4.parse_args(['-a'])
    assert arg.delete_all is True, arg.delete_all

    arg = p4.parse_args([])
   
    arg = p4.parse_args(['--color=black'])
    assert arg.color == 'black'

    arg = p4.parse_args(['--color=red'])
    assert arg.color == 'red'

if __name__ == '__main__':
    n = 0
    for name, test in list(globals().items()):
        if name.startswith('test_'):
            print('Running', name)
            test()
            n +=1
    print('Executed %d tests OK' % n)

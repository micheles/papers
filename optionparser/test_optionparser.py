import sys
from optionparser import OptionParser, ParsingError

def expect_error(err, func, *args, **kw):
    """
    Calls a callable with the given arguments and expects a given
    exception type to be raised, otherwise raises a RuntimeError.
    """
    try:
        func(*args, **kw)
    except Exception, e:
        if str(e) != err:
            raise e.__class__, e, sys.exc_info()[2]
    else:
        raise RuntimeError(
            'Exception %s expected, got none' % errclass.__name__)

p1 = OptionParser('''\
usage: %prog args ... [options]
-d, --delete=: delete a file
''')

def test_p1():
    arg = p1.parse_args(['-d', 'foo', 'arg1', 'arg2'])
    assert arg.delete == 'foo'
    assert arg.args == ['arg1', 'arg2']

    arg = p1.parse_args([])
    assert arg.delete == '', arg.delete
    assert arg.args == [], arg.args
    assert not arg, arg

p2 = OptionParser('''
 usage: %prog arg1 args ... [options]
-d, --delete=: delete a file
''')

def test_p2():
    arg = p2.parse_args(['-d', 'foo', 'arg1', 'arg2'])
    assert arg.delete == 'foo', arg.delete
    assert arg.arg1 == 'arg1', arg.arg1
    assert arg.args == ['arg2'], arg.args

    arg = p2.parse_args(['arg1'])
    assert arg.delete == '', arg.delete
    assert arg.args == [], arg.args
    assert arg, arg
    
    expect_error("Received 0 arguments [], expected 1 ['arg1']",
                 p2.parse_args, [])

p3 = OptionParser('''\
USAGE: %prog arg1 [options]
-d, --delete=: delete a file
''')

def test_p3():
    arg = p3.parse_args(['arg1'])
    assert arg.delete == '', arg.delete
    assert arg.arg1 == 'arg1', arg.args

    expect_error("Received 2 arguments ['arg1', 'arg2'], expected 1 ['arg1']",
                 p3.parse_args, ['arg1', 'arg2'])
    
    expect_error("Received 0 arguments [], expected 1 ['arg1']",
                 p3.parse_args, [])

p4 = OptionParser('''\
Usage: %prog [options]
-c, --color=black: set default color
-d, --delete=: delete the given file
-a, --delete-all: delete all files
''')

def test_p4():
    arg = p4.parse_args(['-a'])
    assert arg.delete_all is True, arg.delete_all

    arg = p4.parse_args([])
    assert not arg, arg

    arg = p4.parse_args(['--color=black'])
    assert not arg, arg

    arg = p4.parse_args(['--color=red'])
    assert arg, arg

import os, sys, inspect, ConfigParser

USAGE = '''
usage:
$ python -m call_with_conf %s <conf name>
'''

def ini2dict(fp, Parser=ConfigParser.RawConfigParser):
    'Converts an INI file into a dictionary of dictionaries'
    parser = Parser()
    parser.readfp(fp)
    fp.close()
    return parser._sections

def get_conf_obj(confname, filenames):
    'Returns the configuration object from the given filenames, or None'
    for filename in filenames:
        if os.path.exists(filename):
            if filename.lower().endswith('.ini'): # .ini file
                dic = ini2dict(file(filename))
            else: # assume a valid Python file
                dic = {}; execfile(filename, dic) 
            return dic.get(confname)

# XXX: the reader could collect names ending in ``_conf`` and display the
# available configurations
def read(module, confname):
    'Look for confname in the default configuration files and in the module'
    appname = module.__name__.split('.')[-1]
    environment_var = appname.upper() + "_STARTUP"
    filename = os.environ.get(environment_var) 
    inifile = os.path.expanduser('~/.%s.ini' % appname)
    rcfile = os.path.expanduser('~/.%src' % appname)
    files = [f for f in [filename, inifile, rcfile] if f]
    files_str = '\n'.join(files + [module.__file__])
    conf_obj = get_conf_obj(confname, files) or getattr(module, confname, None)
    if conf_obj is None:
        sys.exit('Error: no configuration dictionary named %s in files\n%s' % (
            confname, files_str))
    return conf_obj
    
def runapp(name):
    '''
    Run an application (i.e. a Python module or package with a main
    routine) by looking at the sys.argv list. The
    first argument is the application name in the Python dotted
    notation (possibly followed by :<routine-name>) whereas the second
    argument is the name of the configuration objects(no dots there).
    '''
    try:
        name, main_name = name.split(':')
    except ValueError:
        main_name = 'main'
    try:
        main_module = __import__(name, globals(), locals(), [''])
    except ImportError, e:
        sys.exit('ImportError: %s' % e)
    try:
        main = getattr(main_module, main_name)
    except AttributeError:
        sys.exit("NameError: no %s in module %s" % (main_name, name))
    try:
        confname = sys.argv[1]
    except IndexError:
        args = inspect.getargspec(main)[0]
        if args: # main requires args which were not passed
            sys.exit((main_module.__doc__ or '') + (USAGE % name))
        return main()
    return main(read(main_module, confname))

if __name__ == '__main__':
    del sys.argv[0] # remove call_with_conf.py from the argument list
    if not sys.argv:
        sys.exit('Please give application name and configuration name')
    runapp(sys.argv[0])

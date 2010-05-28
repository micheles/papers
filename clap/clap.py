##########################     LICENCE     ###############################
##
##   Copyright (c) 2010, Michele Simionato
##   All rights reserved.
##
##   Redistributions of source code must retain the above copyright 
##   notice, this list of conditions and the following disclaimer.
##   Redistributions in bytecode form must reproduce the above copyright
##   notice, this list of conditions and the following disclaimer in
##   the documentation and/or other materials provided with the
##   distribution. 

##   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
##   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
##   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
##   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
##   HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
##   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
##   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
##   OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
##   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
##   TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
##   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
##   DAMAGE.

"""
CLAP, the smart and simple Command Line Arguments Parser.
See clap/doc.html for the documentation.
"""

import optparse, re, sys, string

class RegexContainer(object):
    """
    A regular expression container for named regexps. 
    All its regular attributes must be valid regular expression templates.
    Notice that the termination character $ must be doubled in
    order to avoid confusion with the template syntax.
    """
    def __init__(self):
        self._dic = {} # a dictionary {name: interpolated regex pattern}
    def __setattr__(self, name, value):
        if not name.startswith('_'): # regular attribute
            templ = string.Template('(?P<' + name + '>' + value + ')')
            pattern = templ.substitute(self._dic)
            object.__setattr__(self, name, re.compile(pattern))
            self._dic[name] = pattern
        else: # private or special attribute
            object.__setattr__(self, name, value)

rx = RegexContainer() # a few regular expressions to parse the usage string

rx.argument = r'[a-zA-Z]\w*'
rx.arguments = '(?:\s+$argument)*'
rx.ellipsis = r'\.\.\.'
rx.prog = r'%prog$arguments\s*$ellipsis?'

rx.enddef = r'\n[ \t]*\n|$$'
rx.lines = r'.*?'
rx.short = r'\w'
rx.long = r'[-\w]+'
rx.default = r'[^:]*'
rx.help = r'.*'

rx.usage = r'(?is)\s*usage:$lines$enddef' # case-insensitive multiline rx
rx.optiondef = r'\s*-$short\s*,\s*--$long\s*=\s*$default:\s*$help'
rx.flagdef = r'\s*-$short\s*,\s*--$long\s*:\s*$help'

def parse_usage(txt):
    "An utility to extract the expected arguments and the rest argument, if any"
    match = rx.prog.match(txt)
    if match is None:
        ParsingError.raise_(txt)
    expected_args = match.group('arguments').split()
    if match.group('ellipsis'):
        return expected_args[:-1], match.group('argument')
    else:
        return expected_args, ''

class ParsingError(Exception):
    @classmethod
    def raise_(cls, usage):
        raise cls("""Wrong format for the usage message.\n\n%s\n
            It should be '%%prog arguments ... [options]""" % usage)

def make_get_default_values(defaults):
    # the purpose of this trick is to allow the idiom
    # if not arg: OptionParser.exit()
    def __nonzero__(self):
        "True if at least one option is set to a non-trivial value"
        for k, v in vars(self).iteritems():
            if v and v != defaults[k]: return True
        return False
    Values = type('Values', (optparse.Values, object), 
                  dict(__nonzero__=__nonzero__))
    return lambda : Values(defaults)

optionstring = None # singleton

class OptionParser(object):
    """
    There should be only one instance of it.
    Attributes: all_options, expected_args, rest_arg, p
    """
    def __init__(self, doc):
        "Populate the option parser."
        global optionstring
        assert doc is not None, \
               "Missing usage string (maybe __doc__ is None)"
        optionstring = doc.replace('%prog', sys.argv[0])

        # parse the doc
        match = rx.usage.search(doc)
        if not match:
            raise ParsingError("Could not find the option definitions")
        optlines = match.group("lines").splitlines()
        prog = optlines[0] # first line
        match = rx.prog.search(prog)
        if not match:
            ParsingError.raise_(prog)
        self.expected_args, self.rest_arg = parse_usage(match.group())
        self.p = optparse.OptionParser(prog)

        # manage the default values
        df = self.p.defaults
        for a in self.expected_args:
            df[a] = None
        if self.rest_arg:
            df[self.rest_arg] = []
        self.p.get_default_values = make_get_default_values(df)

        # parse the options
        for line in optlines[1:]:
            # check if the line is an option definition
            match_option = rx.optiondef.match(line) 
            if match_option:
                action = 'store'
                short, long_,  help, default=match_option.group(
                    "short", "long", "help", "default")
            else: # check if the line is a flag definition
                match_flag = rx.flagdef.match(line)
                if match_flag:
                    action = 'store_true'
                    short, long_, help, default=match_flag.group(
                        "short", "long", "help") + (False,)
                else: # raise an error
                    raise ParsingError(
                        "Cannot parse the definition %r correctly" % line)
            # add the options
            long_ = long_.replace('-', '_')
            self.p.add_option("-" + short, "--" + long_,
                              action=action, help=help, default=default)
        # skip the help option, which destination is None
        self.all_options = [o for o in self.p.option_list if o.dest]
    
    def parse_args(self, arglist=None):
        """
        Parse the received arguments and returns an ``optparse.Values``
        object containing both the options and the positional arguments.
        """
        option, args = self.p.parse_args(arglist)
        n_expected_args = len(self.expected_args)
        n_received_args = len(args)
        if (n_received_args < n_expected_args) or (
            n_received_args > n_expected_args and not self.rest_arg):
            raise ParsingError(
                'Received %d arguments %s, expected %d %s' %
                (n_received_args, args, n_expected_args, self.expected_args))
        for name, value in zip(self.expected_args, args):
            setattr(option, name, value)
        if self.rest_arg:
            setattr(option, self.rest_arg, args[n_expected_args:])
        return option

    @classmethod
    def exit(cls, msg=None):
        exit(msg)

def call(func, args=None, doc=None):
    """
    Magically calls func by passing to it the command lines arguments,
    parsed according to the docstring of func.
    """
    if args is None:
        args = sys.argv[1:]
    if doc is None:
        doc = func.__doc__
    try:
        arg = OptionParser(doc).parse_args(args)
    except ParsingError, e:
        print 'ParsingError:', e
        OptionParser.exit()
    return func(**vars(arg))

def exit(msg=None):
    if msg is None:
        msg = optionstring
    raise SystemExit(msg)

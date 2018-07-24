import os, re, subprocess

LATEX_EXPR = re.compile(r'\$.*\$')

def indent(text):
    return '\n  ' + '\n  '.join(text.splitlines())

def latex2mathml(latex):    
    itex2MML = subprocess.Popen(
        ['./itex2MML'], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    out, err = itex2MML.communicate(latex)
    assert not err, err
    return '\n.. raw:: html\n%s  ' % indent(out)

def rst2s5(rst):
    converter = subprocess.Popen(
        ['rst2s5'], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    out, err = converter.communicate(rst)
    assert not err, err
    return out

def convertfile(fname):    
    rst = LATEX_EXPR.sub(
        lambda m: latex2mathml(m.group()), file('%s.txt' % fname).read())
    print >> file('%s.html' % fname, 'w'), rst2s5(rst),

if __name__ == '__main__':
    convertfile('formulas')
    

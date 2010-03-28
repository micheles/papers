"""An example script invoking optionparse.

  usage: %prog [options] args
  -p, --positional: print positional arguments
  -1, --option1=OPTION1: print option1
  -2, --option2=OPTION2: print option2
"""

import optionparse
opt, args = optionparse.parse(__doc__)
if not opt and not args:
    optionparse.exit()
if opt.positional:
    print args
if opt.option1:
    print opt.option1
if opt.option2:
    print opt.option2

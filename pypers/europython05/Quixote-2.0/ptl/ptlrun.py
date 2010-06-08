#!/usr/bin/env python
import sys
from quixote.ptl.ptl_compile import compile_template
exec compile_template(open(sys.argv[1]), sys.argv[1])


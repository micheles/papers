"""$URL: svn+ssh://svn.mems-exchange.org/repos/trunk/quixote/form/__init__.py $
$Id: __init__.py 26469 2005-04-05 10:26:03Z dbinger $

The web interface framework, consisting of Form and Widget base classes
(and a bunch of standard widget classes recognized by Form).
Application developers will typically create a Form instance each
form in their application; each form object will contain a number
of widget objects.  Custom widgets can be created by inheriting
and/or composing the standard widget classes.
"""

from quixote.form.form import Form, FormTokenWidget
from quixote.form.widget import Widget, StringWidget, FileWidget, \
    PasswordWidget, TextWidget, CheckboxWidget, RadiobuttonsWidget, \
    SingleSelectWidget, SelectWidget, OptionSelectWidget, \
    MultipleSelectWidget, SubmitWidget, HiddenWidget, \
    FloatWidget, IntWidget, subname, WidgetValueError, CompositeWidget, \
    WidgetList, WidgetDict

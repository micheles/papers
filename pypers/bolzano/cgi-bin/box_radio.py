#!/usr/bin/python2.4
import os, sys, cgi
from ms.html_utils import cgipage, cgiform, getscriptname, interp, html_form

def entry(form):
    checkbox="""
    C1<input type=checkbox name=check1><br/>
    C2<input type=checkbox name=check2><br/>
    C3<input type=checkbox name=check3><br/>
    """
    radiobuttons="""
    R1<input type=radio name=choice value=1><br/>
    R2<input type=radio name=choice value=2><br/>
    R3<input type=radio name=choice value=3><br/>
    """
    submit="""
    <input type='submit' name='submit' value='confirm'>
    """
    return cgipage(html_form(checkbox + radiobuttons + submit))

def exit(form):
    return cgipage(interp("""Thank you. You submitted:<br/>
    check1 $check1<br/>
    check2 $check2<br/>
    check3 $check3<br/>
    choice $choice<br/>
    """, form))

if __name__ == "__main__":
    print cgiform(entry, exit)

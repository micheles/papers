#!/usr/bin/python2.4
import os, sys, cgi
from ms.html_utils import cgipage, cgiform, getscriptname, interp, html_form

def entry(form):
    textarea="""
    <textarea name='text'>
    Write something here
    </textarea>
    """
    submit="""
    <input type='submit' name='submit' value='confirm'>
    """
    return cgipage(html_form(textarea + submit))

def exit(form):
    return cgipage(interp("""Thank you. You submitted:<br/>
    $text
    """, dict((k, form.getfirst(k)) for k in form)))

if __name__ == "__main__":
    print cgiform(entry, exit)

#!/usr/bin/env python
import cgi
print "Content-type: text/plain\n"


form = cgi.FieldStorage()
number = form["phonenumber"].value
print "The submitted phone number (%s) has been saved!" % number

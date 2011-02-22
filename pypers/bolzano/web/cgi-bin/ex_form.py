#!/usr/bin/env python
print "Content-type: text/html\n"
form = """
<html>
<head>
</head>
<body>
<form action="save_phonenumber.py">
<input type="text" name="phonenumber" value="XXXXX"><br/>
<input type="submit" name="submit" value="ok">
</form>
</body>
</html>
"""
print form

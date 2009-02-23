from urllib import urlencode
from httplib2 import Http
 
def request(url, meth, data='', user=None, passwd=None):
    headers={'content-type' : 'application/x-www-form-urlencoded'}
    h = Http()
    if user:
        h.add_credentials(user, passwd)
    return h.request(url, meth, urlencode(data), headers)
              
if __name__ == '__main__':
    for meth in ("POST", "PUT", "GET", "DELETE"):
        response, content = request(
            'http://localhost:8080/pizza/employee1', meth,
            dict(pizza='margherita', drink='sprite'))
        print response
        print content

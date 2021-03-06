"""quixote.http_response
$HeadURL: svn+ssh://svn.mems-exchange.org/repos/trunk/quixote/http_response.py $
$Id: http_response.py 26251 2005-02-25 16:17:06Z dbinger $

Provides the HTTPResponse class.
"""

import time
from sets import Set
try:
    import zlib
except ImportError:
    pass
import struct
from rfc822 import formatdate
from quixote.html import stringify

status_reasons = {
    100: 'Continue',
    101: 'Switching Protocols',
    102: 'Processing',
    200: 'OK',
    201: 'Created',
    202: 'Accepted',
    203: 'Non-Authoritative Information',
    204: 'No Content',
    205: 'Reset Content',
    206: 'Partial Content',
    207: 'Multi-Status',
    300: 'Multiple Choices',
    301: 'Moved Permanently',
    302: 'Moved Temporarily',
    303: 'See Other',
    304: 'Not Modified',
    305: 'Use Proxy',
    307: 'Temporary Redirect',
    400: 'Bad Request',
    401: 'Unauthorized',
    402: 'Payment Required',
    403: 'Forbidden',
    404: 'Not Found',
    405: 'Method Not Allowed',
    406: 'Not Acceptable',
    407: 'Proxy Authentication Required',
    408: 'Request Time-out',
    409: 'Conflict',
    410: 'Gone',
    411: 'Length Required',
    412: 'Precondition Failed',
    413: 'Request Entity Too Large',
    414: 'Request-URI Too Large',
    415: 'Unsupported Media Type',
    416: 'Requested range not satisfiable',
    417: 'Expectation Failed',
    422: 'Unprocessable Entity',
    423: 'Locked',
    424: 'Failed Dependency',
    500: 'Internal Server Error',
    501: 'Not Implemented',
    502: 'Bad Gateway',
    503: 'Service Unavailable',
    504: 'Gateway Time-out',
    505: 'HTTP Version not supported',
    507: 'Insufficient Storage',
}

_GZIP_HEADER = ("\037\213" # magic
                "\010" # compression method
                "\000" # flags
                "\000\000\000\000" # time, who cares?
                "\002"
                "\377")

_GZIP_EXCLUDE = Set(["application/pdf",
                     "application/zip",
                     "audio/mpeg",
                     "image/gif",
                     "image/jpeg",
                     "image/png",
                     "video/mpeg",
                     "video/quicktime",
                     "video/x-msvideo",
                     ])

class HTTPResponse:
    """
    An object representation of an HTTP response.

    The Response type encapsulates all possible responses to HTTP
    requests.  Responses are normally created by the Quixote publisher
    or by the HTTPRequest class (every request must have a response,
    after all).

    Instance attributes:
      content_type : string
        the MIME content type of the response (does not include extra params
        like charset)
      charset : string
        the character encoding of the the response
      status_code : int
        HTTP response status code (integer between 100 and 599)
      reason_phrase : string
        the reason phrase that accompanies status_code (usually
        set automatically by the set_status() method)
      headers : { string : string }
        most of the headers included with the response; every header set
        by 'set_header()' goes here.  Does not include "Status" or
        "Set-Cookie" headers (unless someone uses set_header() to set
        them, but that would be foolish).
      body : str | Stream
        the response body, None by default.  Note that if the body is not a
        stream then it is already encoded using 'charset'.
      buffered : bool
        if false, response data will be flushed as soon as it is
        written (the default is true).  This is most useful for
        responses that use the Stream() protocol.  Note that whether the
        client actually receives the partial response data is highly
        dependent on the web server
      cookies : { name:string : { attrname : value } }
        collection of cookies to set in this response; it is expected
        that the user-agent will remember the cookies and send them on
        future requests.  The cookie value is stored as the "value"
        attribute.  The other attributes are as specified by RFC 2109.
      cache : int | None
        the number of seconds the response may be cached.  The default is 0,
        meaning don't cache at all.  This variable is used to set the HTTP
        expires header.  If set to None then the expires header will not be
        added.
      javascript_code : { string : string }
        a collection of snippets of JavaScript code to be included in
        the response.  The collection is built by calling add_javascript(),
        but actually including the code in the HTML document is somebody
        else's problem.
    """

    DEFAULT_CONTENT_TYPE = 'text/html'
    DEFAULT_CHARSET = 'iso-8859-1'
    
    def __init__(self, status=200, body=None, content_type=None, charset=None):
        """
        Creates a new HTTP response.
        """
        self.content_type = content_type or self.DEFAULT_CONTENT_TYPE
        self.charset = charset or self.DEFAULT_CHARSET
        self.set_status(status)
        self.headers = {}

        if body is not None:
            self.set_body(body)
        else:
            self.body = None

        self.cookies = {}
        self.cache = 0
        self.buffered = True
        self.javascript_code = None

    def set_content_type(self, content_type, charset='iso-8859-1'):
        """(content_type : string, charset : string = 'iso-8859-1')

        Set the content type of the response to the MIME type specified by
        'content_type'.  Also sets the charset, defaulting to 'iso-8859-1'.
        """
        self.charset = charset
        self.content_type = content_type

    def set_charset(self, charset):
        self.charset = str(charset).lower()

    def set_status(self, status, reason=None):
        """set_status(status : int, reason : string = None)

        Sets the HTTP status code of the response.  'status' must be an
        integer in the range 100 .. 599.  'reason' must be a string; if
        not supplied, the default reason phrase for 'status' will be
        used.  If 'status' is a non-standard status code, the generic
        reason phrase for its group of status codes will be used; eg.
        if status == 493, the reason for status 400 will be used.
        """
        if not isinstance(status, int):
            raise TypeError, "status must be an integer"
        if not (100 <= status <= 599):
            raise ValueError, "status must be between 100 and 599"

        self.status_code = status
        if reason is None:
            if status_reasons.has_key(status):
                reason = status_reasons[status]
            else:
                # Eg. for generic 4xx failures, use the reason
                # associated with status 400.
                reason = status_reasons[status - (status % 100)]
        else:
            reason = str(reason)

        self.reason_phrase = reason

    def set_header(self, name, value):
        """set_header(name : string, value : string)

        Sets an HTTP return header "name" with value "value", clearing
        the previous value set for the header, if one exists.
        """
        self.headers[name.lower()] = value

    def get_header(self, name, default=None):
        """get_header(name : string, default=None) -> value : string

        Gets an HTTP return header "name".  If none exists then 'default' is
        returned.
        """
        return self.headers.get(name.lower(), default)

    def set_expires(self, seconds=0, minutes=0, hours=0, days=0):
        if seconds is None:
            self.cache = None # don't generate 'Expires' header
        else:
            self.cache = seconds + 60*(minutes + 60*(hours + 24*days))

    def _encode_chunk(self, chunk):
        """(chunk : str | unicode) -> str
        """
        if self.charset == 'iso-8859-1' and isinstance(chunk, str):
            return chunk # non-ASCII chars are okay
        else:
            return chunk.encode(self.charset)

    def _compress_body(self, body):
        """(body: str) -> str
        """
        n = len(body)
        co = zlib.compressobj(6, zlib.DEFLATED, -zlib.MAX_WBITS,
                              zlib.DEF_MEM_LEVEL, 0)
        chunks = [_GZIP_HEADER,
                  co.compress(body),
                  co.flush(),
                  struct.pack("<ll", zlib.crc32(body), n)]
        compressed_body = "".join(chunks)
        ratio = float(n) / len(compressed_body)
        #print "gzip original size %d, ratio %.1f" % (n, ratio)
        if ratio > 1.0:
            self.set_header("Content-Encoding", "gzip")
            return compressed_body
        else:
            return body

    def set_body(self, body, compress=False):
        """(body : any, compress : bool = False)

        Sets the response body equal to the argument 'body'.  If 'compress'
        is true then the body may be compressed using 'gzip'.
        """
        if not isinstance(body, Stream):
            body = self._encode_chunk(stringify(body))
            if compress and self.content_type not in _GZIP_EXCLUDE:
                body = self._compress_body(body)
        self.body = body

    def expire_cookie(self, name, **attrs):
        """
        Cause an HTTP cookie to be removed from the browser

        The response will include an HTTP header that will remove the cookie
        corresponding to "name" on the client, if one exists.  This is
        accomplished by sending a new cookie with an expiration date
        that has already passed.  Note that some clients require a path
        to be specified - this path must exactly match the path given
        when creating the cookie.  The path can be specified as a keyword
        argument.
        """
        dict = {'max_age': 0, 'expires': 'Thu, 01-Jan-1970 00:00:00 GMT'}
        dict.update(attrs)
        self.set_cookie(name, "deleted", **dict)

    def set_cookie(self, name, value, **attrs):
        """set_cookie(name : string, value : string, **attrs)

        Set an HTTP cookie on the browser.

        The response will include an HTTP header that sets a cookie on
        cookie-enabled browsers with a key "name" and value "value".
        Cookie attributes such as "expires" and "domains" may be
        supplied as keyword arguments; see RFC 2109 for a full list.
        (For the "secure" attribute, use any true value.)

        This overrides any previous value for this cookie.  Any
        previously-set attributes for the cookie are preserved, unless
        they are explicitly overridden with keyword arguments to this
        call.
        """
        cookies = self.cookies
        if cookies.has_key(name):
            cookie = cookies[name]
        else:
            cookie = cookies[name] = {}
        cookie.update(attrs)
        cookie['value'] = value

    def add_javascript(self, code_id, code):
        """Add javascript code to be included in the response.

        code_id is used to ensure that the same piece of code is not
        included twice.  The caller must be careful to avoid
        unintentional code_id and javascript identifier collisions.
        Note that the response object only provides a mechanism for
        collecting code -- actually including it in the HTML document
        that is the response body is somebody else's problem.  (For
        an example, see Form._render_javascript().)
        """
        if self.javascript_code is None:
            self.javascript_code = {code_id: code}
        elif not self.javascript_code.has_key(code_id):
            self.javascript_code[code_id] = code

    def redirect(self, location, permanent=False):
        """Cause a redirection without raising an error"""
        if not isinstance(location, str):
            raise TypeError, "location must be a string (got %s)" % `location`
        # Ensure that location is a full URL
        if location.find('://') == -1:
            raise ValueError, "URL must include the server name"
        if permanent:
            status = 301
        else:
            status = 302
        self.set_status(status)
        self.headers['location'] = location
        self.set_content_type('text/plain')
        return "Your browser should have redirected you to %s" % location

    def get_status_code(self):
        return self.status_code

    def get_reason_phrase(self):
        return self.reason_phrase

    def get_content_type(self):
        return self.content_type

    def get_content_length(self):
        if self.body is None:
            return None
        elif isinstance(self.body, Stream):
            return self.body.length
        else:
            return len(self.body)

    def _gen_cookie_headers(self):
        """_gen_cookie_headers() -> [string]

        Build a list of "Set-Cookie" headers based on all cookies
        set with 'set_cookie()', and return that list.
        """
        cookie_headers = []
        for name, attrs in self.cookies.items():
            value = str(attrs['value'])
            if '"' in value:
                value = value.replace('"', '\\"')
            chunks = ['%s="%s"' % (name, value)]
            for name, val in attrs.items():
                name = name.lower()
                if val is None:
                    continue
                if name in ('expires', 'domain', 'path', 'max_age', 'comment'):
                    name = name.replace('_', '-')
                    chunks.append('%s=%s' % (name, val))
                elif name == 'secure' and val:
                    chunks.append("secure")
            cookie_headers.append(("Set-Cookie", '; '.join(chunks)))
        return cookie_headers

    def generate_headers(self):
        """generate_headers() -> [(name:string, value:string)]

        Generate a list of headers to be returned as part of the response.
        """
        headers = []

        for name, value in self.headers.items():
            headers.append((name.title(), value))

        # All the "Set-Cookie" headers.
        if self.cookies:
            headers.extend(self._gen_cookie_headers())

        # Date header
        now = time.time()
        if "date" not in self.headers:
            headers.append(("Date", formatdate(now)))

        # Cache directives
        if self.cache is None:
            pass # don't mess with the expires header
        elif "expires" not in self.headers:
            if self.cache > 0:
                expire_date = formatdate(now + self.cache)
            else:
                expire_date = "-1" # allowed by HTTP spec and may work better
                                   # with some clients
            headers.append(("Expires", expire_date))

        # Content-type
        if "content-type" not in self.headers:
            headers.append(('Content-Type',
                            '%s; charset=%s' % (self.content_type,
                                                self.charset)))

        # Content-Length
        if "content-length" not in self.headers:
            length = self.get_content_length()
            if length is not None:
                headers.append(('Content-Length', length))

        return headers

    def generate_body_chunks(self):
        """Return a sequence of body chunks, encoded using 'charset'.
        """
        if self.body is None:
            pass
        elif isinstance(self.body, Stream):
            for chunk in self.body:
                yield self._encode_chunk(chunk)
        else:
            yield self.body # already encoded

    def write(self, output, include_status=True):
        """(output : file, include_status : bool = True)

        Write the HTTP response headers and body to 'output'.  This is not
        a complete HTTP response, as it doesn't start with a response
        status line as specified by RFC 2616.  By default, it does start
        with a "Status" header as described by the CGI spec.  It is expected
        that this response is parsed by the web server and turned into a
        complete HTTP response.
        """
        flush_output = not self.buffered and hasattr(output, 'flush')
        if include_status:
            # "Status" header must come first.
            output.write("Status: %03d %s\r\n" % (self.status_code,
                                                  self.reason_phrase))
        for name, value in self.generate_headers():
            output.write("%s: %s\r\n" % (name, value))
        output.write("\r\n")
        if flush_output:
            output.flush()
        for chunk in self.generate_body_chunks():
            output.write(chunk)
            if flush_output:
                output.flush()
        if flush_output:
            output.flush()


class Stream:
    """
    A wrapper around response data that can be streamed.  The 'iterable'
    argument must support the iteration protocol.  Items returned by 'next()'
    must be strings.  Beware that exceptions raised while writing the stream
    will not be handled gracefully.

    Instance attributes:
      iterable : any
        an object that supports the iteration protocol.  The items produced
        by the stream must be strings.
      length: int | None
        the number of bytes that will be produced by the stream, None
        if it is not known.  Used to set the Content-Length header.
    """
    def __init__(self, iterable, length=None):
        self.iterable = iterable
        self.length = length

    def __iter__(self):
        return iter(self.iterable)

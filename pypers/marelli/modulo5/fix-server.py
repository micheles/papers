
############ FIX A MISSING FEATURE IN HTTPServer ################

from BaseHTTPServer import HTTPServer

# The only exception that can propagate here is SystemExit
def handle_exit(self, request, client_address):
    raise SystemExit("Server is shutting down")

HTTPServer.handle_error = handle_exit

############ FIX A MISSING FEATURE IN Publisher ################

import time
from quixote.publish import PublishError

def process_request(self, request):
        """(request : HTTPRequest) -> HTTPResponse

        Process a single request, given an HTTPRequest object.  The
        try_publish() method will be called to do the work and
        exceptions will be handled here.
        """
        self._set_request(request)
        start_time = time.time()
        try:
            self.parse_request(request)
            output = self.try_publish(request)
        except PublishError, exc:
            # Exit the publishing loop and return a result right away.
            output = self.finish_interrupted_request(exc)
        except SystemExit: # <====  THIS IS
             raise         # <====  THE FIX
        except:
            # Some other exception, generate error messages to the logs, etc.
            output = self.finish_failed_request()
        output = self.filter_output(request, output)
        self.logger.log_request(request, start_time)
        if output:
            if self.config.compress_pages and request.get_encoding(["gzip"]):
                compress = True
            else:
                compress = False
            request.response.set_body(output, compress)
        self._clear_request()
        return request.response

Publisher.process_request = process_request

from ms.quixote_utils_exp import Website, htmlpage, FormPage
from doctester import runtests
from StringIO import StringIO
import sys

class DoctestPage(FormPage):
    form_config = """\
    [text]
    name: txt
    title: Doctester input
    
    [checkbox]
    name: verbose
    title: Verbose
    
    [submit]
    name: test
    value: test!"""
            
    @htmlpage()
    def exitpage(self):
        if self.form["verbose"]:
            yield "<pre>%s</pre>" % self.out.getvalue()
        else:
            yield "%(tests)s tests, %(fail)s failed" % vars(self)
    
    @htmlpage()
    def errorpage(self):
        yield self.form.get_widget('txt').error
        yield "<pre>%s</pre>" % self.out.getvalue()
   
    def checker(self):
        sys.stdout_orig = sys.stdout
        sys.stdout = self.out = StringIO()
        txt, verbose = self.form["txt"] or "", self.form["verbose"]
        self.fail, self.tests = runtests(txt, verbose=verbose)
        sys.stdout = sys.stdout_orig
        if self.fail:
            self.form.set_error("txt", "Doctester error")
        
publisher = Website(_q_index=DoctestPage("doctester")).publisher()

if __name__ == "__main__":
    publisher.run_show(port=7080)

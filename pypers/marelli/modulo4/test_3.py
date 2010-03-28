from tkMessageBox import showinfo

class Test1(object):
    def setup_class(cls):
        pass
        #showinfo("setup", "setup")
    def teardown_class(cls):
        print "teardown"
    def test_m1(self):
        assert 1==0
    def test_m2(self):
        assert 1==0

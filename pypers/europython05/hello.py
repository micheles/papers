from ms.quixote_utils_exp import Website, htmlpage

@htmlpage()
def hello():
    return "hello world"

Website(hello).publisher().run_show(page="hello",port=7080)

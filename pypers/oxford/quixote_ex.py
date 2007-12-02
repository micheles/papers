from ms.quixote_utils_exp import Website, htmlpage

@htmlpage()
def _q_index():
    yield "This is the main page."
    yield "You can access <a href='apage'>apage</a> too."

@htmlpage()
def apage():
    return "hello!"

publisher = Website(_q_index, apage).publisher()

if __name__ == "__main__":
    publisher.run_show()

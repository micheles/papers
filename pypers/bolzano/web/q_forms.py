from quixote_utils import RootDirectory
from quixote.form.widget import StringWidget, SubmitWidget
from quixote.form.form import Form

class Root(RootDirectory):
    _q_exports = ["string_widget", "submit_button", "form_with_submit",
                  "thank_you"]

    def string_widget(self):
        return StringWidget("Telefono", value="0699944857").render()

    def submit_button(self):
        return SubmitWidget("submit", value="ok").render()

    def thank_you(self, number):
        return "Thank you for submitting %s" % number
    
    def form_with_submit(self):
        form = Form()
        form.add(StringWidget, "Telefono", value="XXX")
        form.add(SubmitWidget, "submit", value="ok")
        if not form.is_submitted():
            return form.render()
        else:
            return self.thank_you(form["Telefono"])

Root().publish_show("form_with_submit")

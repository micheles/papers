from jquery_helper import Dispatcher
from paste.httpserver import serve

root = ('''
<link rel="stylesheet" media="all" type="text/css" href="/static/mainmenu.css"/>
<div class="menu">

<ul>
 <li><img src="/mb-home.gif" width="76" height="18" alt="[Home]" title="" />
		<ul >
			<li><a href="/purchasing.htm">&nbsp;Purchasing</a></li>
			<li><a href="/default.htm#bottom">&nbsp;Sales Support</a></li>
			<li><a href="/support.htm">&nbsp;Technical Support</a></li>

			<li><a href="/default.htm#bottom">&nbsp;Contact Us</a></li>
			<li><a href="http://www.grcmail.com/mail.htm">&nbsp;Mailing List</a></li>
			<li><a href="/privacy.htm">&nbsp;Privacy Policy</a></li>
			<li><a href="/siteoptions.htm">&nbsp;Site Options</a></li>
			<li><a href="/stevegibson.htm">&nbsp;Steve's Projects Page</a></li>
			<li><a href="/resume.htm">&nbsp;Steve's Old Resume</a></li>

		</ul>
	</li>
</ul>

<ul>
 <li><img src="/mb-products.gif" width="96" height="18" alt="[Products]" title="" />
		<ul>
			<li><a href="/sr/spinrite.htm">&nbsp;General information</a></li>
			<li><a href="/sr/testimonials.htm">&nbsp;User testimonials</a></li>

			<li><a href="/cs/prepurch.htm">&nbsp;Purchase SpinRite</a></li>
			<li><a href="/sr/faq.htm">&nbsp;FAQ</a></li>
			<li><a href="/sr/themovie.htm">&nbsp;Demo Videos</a></li>
			<li><a href="/sr/kb/sata.htm">&nbsp;Knowledgebase: SATA</a></li>
			<li><a href="/sr/kb/badbios.htm">&nbsp;Knowledgebase: BIOS</a></li>
			<li><a href="/sroverview.htm">&nbsp;SpinRite v5.0 pages</a></li>

		</ul>
	</li>
</ul>

<ul>
     <li><img src="/mb-services.gif" width="94" height="18" alt="[Services]" title="" />
		<ul>
			<li><a href="https://www.grc.com/x/ne.dll?bh0bkyd2">&nbsp;ShieldsUP!</a></li>
			<li><a href="/securitynow.htm">&nbsp;Security Now!</a></li>

			<li><a href="https://www.grc.com/passwords.htm">&nbsp;Perfect Passwords</a></li>
			<li><a href="https://www.grc.com/ppp.htm">&nbsp;PPP Passwords</a></li>
			<li><a href="/media.htm">&nbsp;Tech TV video clips</a></li>
			<li><a href="/discussions.htm">&nbsp;Newsgroup Discussions</a></li>
		</ul>
	</li>

</ul>
</div> <!-- close "menu" div -->
<hr style="display:none" />
Prova

''', '')

if __name__ == '__main__':
    serve(Dispatcher('/tmp', root), '', 8000)

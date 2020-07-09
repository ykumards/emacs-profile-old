(setq org-static-blog-publish-title "Bastibe.de")
(setq org-static-blog-publish-url "https://bastibe.de/")
(setq org-static-blog-publish-directory "~/")
(setq org-static-blog-posts-directory "~/posts/")
(setq org-static-blog-drafts-directory "~/drafts/")
;;(setq org-static-blog-publish-directory "~/Documents/Work/personal_site/ykumards.github.io/")
;;(setq org-static-blog-posts-directory "~/Documents/Work/personal_site/ykumards.github.io/posts/")
;;(setq org-static-blog-drafts-directory "~/Documents/Work/personal_site/ykumards.github.io/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

(setq org-static-blog-page-header
"<meta name=\"author\" content=\"Yogesh Kumar\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"css/org-notes-style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">
<link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
<link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
<link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
<script src=\"static/katex.min.js\"></script>
<script src=\"static/auto-render.min.js\"></script>
<link rel=\"stylesheet\" href=\"static/katex.min.css\">
<script>document.addEventListener(\"DOMContentLoaded\", function() { renderMathInElement(document.body); });</script>
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"https://ykumards.github.io\">Le Blog</a>
  <div class=\"sitelinks\">
    <a href=\"https://twitter.com/ykumards\">Twitter</a> | <a href=\"https://github.com/ykumards\">Github</a>
  </div>
</div>")

(setq org-static-blog-page-postamble
"<div id=\"archive\">
  <a href=\"https://ykumards.github.io/archive.html\">Other posts</a>
</div>
<center><button id=\"disqus_button\" onclick=\"load_disqus()\">Load Disqus Comments</button></center>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
    function load_disqus() {
        var dsq = document.createElement('script');
        dsq.type = 'text/javascript';
        dsq.async = true;
        dsq.src = 'https://ykumards-github-io-1.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        document.getElementById('disqus_button').style.visibility = 'hidden';
    };
<noscript>Please enable JavaScript to view the <a href="https://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
</script>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">ykumards.github.io</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://ykumards.github.io\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

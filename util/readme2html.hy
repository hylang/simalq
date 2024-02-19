(import
  pathlib [Path]
  lxml.html)


(setv E (hy.I.lxml/builder.ElementMaker
  :makeelement lxml.html.html-parser.makeelement))

(setv doc (lxml.html.fromstring (hy.I.docutils/core.publish-string
  (.read-bytes (Path "README.rst"))
  :writer-name "html5"
  :settings-overrides (dict :stylesheet-path None))))

; Add some CSS.
(.append (get (.xpath doc "//head") 0) (E.style "
  h1, .subtitle
     {text-align: center}
  h1
     {margin-bottom: 0}
  .subtitle
     {margin-top: 0}
  code
     {background-color: #eee;
      padding: .1em .2em;
      border: thin solid gray;}"))

; Get rid of the viewport finagling.
(setv [x] (.xpath doc "//meta[@name = 'viewport']"))
(.remove (.getparent x) x)

; Remove paragraphs in list items.
(for [x (.xpath doc "//li")]
  (setv [p] x)
  (assert (= p.tag "p"))
  (setv x.text p.text)
  (setv (cut x) (list p)))

; Change code spans to actual code tags.
(for [x (.xpath doc "//span[@class = 'docutils literal']")]
  (setv x.tag "code")
  (.clear x.attrib))

(print (lxml.html.tostring doc
  :pretty-print True :encoding "unicode" :doctype "<!DOCTYPE html>"))

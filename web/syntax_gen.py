#!/usr/bin/env python

TEMPLATE = """
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>Linear Lambda CCS - Syntax</title>
    <style>
      /** Mostly shamelessly stolen from OCamlDoc. Thanks! */

      .tex { color: #465F91; }
      .par { font-weight: bold; font-size: 1.2em; }
      .right { text-align: right; }
      .anchor {}
      .abssyn {}
      .alterns { color: #610259; font-style: italic; }
      .abssynCode { color: #465F91; font-family: "consolas", "sans-serif"; }
      .abssynComment { font-style: italic; padding-left: 50px; }

      .keyword { font-weight : bold ; color : Red }
      .keywordsign { color : #C04600 }
      .comment { color : Green }
      .constructor { color : Blue }
      .type { color : #5C6585 }
      .string { color : Maroon }
      .warning { color : Red ; font-weight : bold }
      .info { margin-left : 3em; margin-right: 3em }
      .param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }
      .code { color : #465F91 ; }
      .typetable { border-style : hidden }
      .paramstable { border-style : hidden ; padding: 5pt 5pt}
      tr { background-color : White }
      td.typefieldcomment { background-color : #FFFFFF ; font-size: smaller ;}
      div.sig_block {margin-left: 2em}
      body {font: 13px sans-serif; color: black; text-align: left;
        padding: 5px; margin: 0}
      h1 { font-size : 20pt ; text-align: center; }
      h2 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #90BDFF;
        padding: 2px; }
      h3 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #90DDFF;
        padding: 2px; }
      h4 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #90EDFF;
        padding: 2px; }
      h5 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #90FDFF;
        padding: 2px; }
      h6 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #90BDFF ;
        padding: 2px; }
      div.h7 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #E0FFFF ;
        padding: 2px; }
      div.h8 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #F0FFFF ;
        padding: 2px; }
      div.h9 { font-size : 20pt ; border: 1px solid #000000; margin-top: 5px;
        margin-bottom: 2px;text-align: center; background-color: #FFFFFF ;
        padding: 2px; }
      a {color: #416DFF; text-decoration: none}
      a:hover {background-color: #ddd; text-decoration: underline}
      pre { margin-bottom: 4px; font-family: monospace; }
      pre.verbatim, pre.codepre { }
      .indextable {border: 1px #ddd solid; border-collapse: collapse}
      .indextable td, .indextable th {border: 1px #ddd solid; min-width: 80px}
      .indextable td.module {background-color: #eee ;  padding-left: 2px;
      padding-right: 2px}
      .indextable td.module a {color: 4E6272; text-decoration: none; display:
        block; width: 100%}
      .indextable td.module a:hover {text-decoration: underline;
        background-color: transparent}
      .deprecated {color: #888; font-style: italic}
      .indextable tr td div.info { margin-left: 2px; margin-right: 2px }
      ul.indexlist { margin-left: 0; padding-left: 0;}
      ul.indexlist li { list-style-type: none ; margin-left: 0;
        padding-left: 0; }
    </style>
  </head>

  <body>
    <h1>Linear λCSS — Syntax</h1>

"""


def anchor(name, val):
    return """<a href="#{}" class="anchor">{}</a>""".format(name, val)


def absSyntax(name, anchor, rules, selfrefs):
    if len(rules) == 0:
        return ""

    template_endline = """
        <td class="abssynCode">{}</td>
        <td class="abssynComment">{}</td>"""
    templateline = """
        <tr>
          <td></td><td class="right">| </td>{}
        </tr>""".format(template_endline)
    out = \
        """
        <div id="{}">
        <p class="par">{}</p>

        <table class="abssyn">
            <tr><td class="right">{}</td><td class="right">::=</td>{}</tr>""" \
            .format(anchor, name, selfrefs,
                    template_endline.format(rules[0][0], rules[0][1]))

    for rule, comment in rules[1:]:
        out += templateline.format(rule, comment)
    return out + "</table></div>"


def hN(num, val):
    return "<h{}>{}</h{}>".format(num, val, num)


def alternatives(altList):
    assert(len(altList) > 0)
    out = """<span class="alterns">&#123;{}&#125;</span>"""
    middle = altList[0]
    for elt in altList[1:]:
        middle += ", {}".format(elt)
    return out.format(middle)

intro = """
    <p>
    In this document, we describe the concrete syntax used in
    the implementation to represent linear lambda-CCS terms.

    In the following expressions,
    <span class="tex">{}</span> denotes alternative
    patterns that can be used in this syntax case: for instance,
    <span class="tex">x {}</span>
    denotes two syntax rules having the same effect, namely
    <span class="tex">x y</span> and <span class="tex">x z</span>.
    </p>
    """.format(alternatives(['a', 'b', 'c']), alternatives(['y', 'z']))

abstractSyntax = hN(2, "Abstract syntax") + '\n\n' \
    + absSyntax("Terms", "syn:terms", [
        ("0", "failure"),
        ("1", "success"),
        (anchor("syn:ident", "x"), "variable identifier"),
        ("(M)", ""),
        ("M || N", "parallel execution"),
        ("M ; N", "sequential execution"),
        ("[{}]-M".format(anchor("syn:channel", "a")), "channel call"),
        ("({}[{}]) M".format(alternatives(['ν', '&']),
                             anchor("syn:channel", "a")),
            "channel introduction"),
        ("{}{} : {} . M".format(alternatives(["λ", "\\", "^"]),
                                anchor("syn:ident", "x"),
                                anchor("syn:type", "T")),
            "lambda abstraction"),
        ("M N", "application")], "M, N") \
    + absSyntax("Channels", "syn:channel", [
        ("a", "channel"),
        ("~a", "dual channel")], "a, b") \
    + absSyntax("Identifiers", "syn:ident", [
        ("[a-z A-Z][a-z A-Z 0-9 _]*", "")], "x, y, z") \
    + absSyntax("Types", "syn:type", [
        ("P", "process"),
        ("C", "channel"),
        ("T {} U".format(alternatives(["→", "->"])), "linear function")],
        "T, U")

print(TEMPLATE + intro + abstractSyntax + """
  </body>
</html>""")

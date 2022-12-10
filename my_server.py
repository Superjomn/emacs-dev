from epc.server import EPCServer
from typing import *

server = EPCServer(('localhost', 0))


def html_template(table: str):
    return r"""
<!doctype html>
<html lang="en">
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap.min.css" integrity="sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu" crossorigin="anonymous">
<style>
    body {
       font-size: 16px;
    }

    table.center {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 80%
    }
    table td {
        width: 45%;
    }
</style>
 <body>
""" \
           + table + \
           """
       </body>
       </html>
           """


class OrgFormatter:
    '''
    Translate the org-mode format to html.
    '''
    style = [
        ('/', 'i'),
        ('*', 'b'),
        ('_', 'u'),
        ('~', 'code'),
    ]

    def __call__(self, text: str):
        for symbol, html_tag_token in OrgFormatter.style:
            text = self.format(text, symbol, html_tag_token)
        return text

    def format(self, content: str, symbol: str, html_tag_token: str):
        '''
        Dealing with **, ~~ and so on.

        symbol: a character like '*' or '_'
        html_tag_token: 'b' or 'u'
        '''
        i = 0

        res = ""
        star = content.find(symbol, i)
        if star == -1: return content
        while star >= i:
            res += content[i:star]
            res += "<%s>" % html_tag_token
            i = star + 1
            star = content.find(symbol, i)
            assert star != -1
            res += content[i:star]
            res += "</%s>" % html_tag_token

            i = star + 1
            star = content.find(symbol, i)
        res += content[i:]
        return res


def binary_lang_to_org_table(lang0: List[str], lang1: List[str]):
    assert len(lang0) == len(lang1)
    n = len(lang0)
    res = []
    res.append('<table class="table center table-bordered table-hover">')
    formatter = OrgFormatter()
    for i in range(n):
        col0 = formatter(lang0[i])
        col1 = formatter(lang1[i])
        row = "<tr><td> %s </td><td> %s </td></tr>" % (col0, col1)
        res.append(row)
    res.append("</table>")

    content = "\n".join(res)
    return html_template(content)


@server.register_function
def bili_double_lang_to_org_table(lang0: str, lang1: str):
    return binary_lang_to_org_table(lang0.split('\n'), lang1.split('\n'))


@server.register_function
def echo(*a):
    return a


if __name__ == '__main__':
    server.print_port()
    server.serve_forever()

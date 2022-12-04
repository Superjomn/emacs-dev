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


def binary_lang_to_org_table(lang0: List[str], lang1: List[str]):
    assert len(lang0) == len(lang1)
    n = len(lang0)
    res = []
    res.append('<table class="table center table-bordered table-hover">')
    for i in range(n):
        row = "<tr><td> %s </td><td> %s </td></tr>" % (lang0[i], lang1[i])
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


server.print_port()
server.serve_forever()

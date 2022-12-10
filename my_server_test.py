from my_server import OrgFormatter


def test_org_format_to_html():
    formatter = OrgFormatter()
    assert formatter("hello *world* yes") == "hello <b>world</b> yes"
    assert formatter("hello _world_ yes") == "hello <u>world</u> yes"

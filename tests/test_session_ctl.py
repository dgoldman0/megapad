"""Focused contracts for shared-session control consumers."""

from __future__ import annotations

import base64
from types import SimpleNamespace

from session_ctl import wait_for_text


class _RawClient:
    def __init__(self, responses):
        self.responses = list(responses)
        self.last_offset = 0

    def request(self, method, **params):
        assert method == "raw"
        assert params["since"] == self.last_offset
        if self.responses:
            result = self.responses.pop(0)
            self.last_offset = result["offset"]
            return result
        return _raw_response(self.last_offset, b"")


def _raw_response(offset: int, payload: bytes, *, truncated: bool = False):
    return {
        "offset": offset,
        "truncated": truncated,
        "data_base64": base64.b64encode(payload).decode("ascii"),
        "text": payload.decode("utf-8", errors="replace"),
    }


def _args(text: str):
    return SimpleNamespace(
        text=text,
        scope="raw",
        timeout=0.03,
        from_now=False,
    )


def test_wait_text_decodes_utf8_across_absolute_raw_chunks(capsys):
    client = _RawClient(
        (
            _raw_response(4, b"caf\xc3"),
            _raw_response(5, b"\xa9"),
        )
    )

    assert wait_for_text(client, _args("caf\N{LATIN SMALL LETTER E WITH ACUTE}")) == 0
    assert '"matched": true' in capsys.readouterr().out


def test_wait_text_cannot_match_across_a_truncated_history_gap(capsys):
    client = _RawClient(
        (
            _raw_response(2, b"ab"),
            _raw_response(4, b"cd", truncated=True),
        )
    )

    assert wait_for_text(client, _args("abcd")) == 1
    output = capsys.readouterr().out
    assert '"matched": false' in output
    assert '"recent": "cd"' in output

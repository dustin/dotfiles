#!/usr/bin/env python3
"""HTTP service that fetches per-plate filament weight info from a Bambu
printer's sliced 3mf plate file over implicit FTPS.

Configuration is via environment variables (populated by the Nix module):
  BAMBU_PRINTER_HOST  - printer hostname/IP (default: a1mini.lan)
  BAMBU_ACCESS_CODE   - printer LAN access code (required)
  BAMBU_LISTEN_HOST   - address to bind the HTTP server to (default: 0.0.0.0)
  BAMBU_LISTEN_PORT   - port to bind the HTTP server to (default: 8788)
"""
import io
import json
import os
import re
import ssl
import sys
import zipfile
import ftplib
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

PRINTER_HOST = os.environ.get("BAMBU_PRINTER_HOST", "a1mini.lan")
ACCESS_CODE = os.environ.get("BAMBU_ACCESS_CODE")
LISTEN_HOST = os.environ.get("BAMBU_LISTEN_HOST", "0.0.0.0")
LISTEN_PORT = int(os.environ.get("BAMBU_LISTEN_PORT", "8788"))

# Subtask names come straight from an HTTP query param and get interpolated
# into an FTP RETR command, so keep them restricted to a safe charset. This
# also happens to match the numeric/alphanumeric job names Bambu uses.
SUBTASK_RE = re.compile(r"^[A-Za-z0-9_.-]+$")


class ImplicitFTP_TLS(ftplib.FTP_TLS):
    """ftplib only speaks explicit FTPS; Bambu printers require implicit TLS
    from the first byte on port 990. This wraps the control socket immediately
    on connect instead of waiting for an AUTH TLS command."""
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._sock = None

    @property
    def sock(self):
        return self._sock

    @sock.setter
    def sock(self, value):
        if value is not None and not isinstance(value, ssl.SSLSocket):
            value = self.context.wrap_socket(value)
        self._sock = value

    def ntransfercmd(self, cmd, rest=None):
        conn, size = ftplib.FTP.ntransfercmd(self, cmd, rest)
        conn = self.context.wrap_socket(conn, server_hostname=self.host, session=self.sock.session)
        return conn, size


def fetch_weight(subtask: str) -> dict:
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE

    ftps = ImplicitFTP_TLS(context=ctx)
    try:
        ftps.connect(host=PRINTER_HOST, port=990, timeout=15)
        ftps.login(user="bblp", passwd=ACCESS_CODE)
        ftps.prot_p()

        buf = io.BytesIO()
        ftps.retrbinary(f"RETR {subtask}.gcode.3mf", buf.write)
    finally:
        try:
            ftps.close()
        except Exception:
            pass
    buf.seek(0)

    with zipfile.ZipFile(buf) as zf:
        xml = zf.read("Metadata/slice_info.config").decode("utf-8")

    # slice_info.config has <filament id=".." type=".." used_g=".." .../> per
    # filament, and a per-plate total weight attribute. Grabbing both with a
    # simple regex rather than a full XML parse, since we only need a couple
    # of numeric fields and it avoids pulling in a parser dependency.
    filaments = [
        {"type": m.group("type"), "used_g": float(m.group("g"))}
        for m in re.finditer(r'<filament[^>]*type="(?P<type>[^"]+)"[^>]*used_g="(?P<g>[\d.]+)"', xml)
    ]
    plate_weight = re.search(r'weight="([\d.]+)"', xml)

    return {
        "subtask": subtask,
        "filaments": filaments,
        "total_weight_g": float(plate_weight.group(1)) if plate_weight else None,
    }


class Handler(BaseHTTPRequestHandler):
    def _respond(self, status: int, payload: bytes, content_type: str = "application/json"):
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    def do_GET(self):
        params = urllib.parse.parse_qs(urllib.parse.urlparse(self.path).query)
        subtask = params.get("subtask", [None])[0]
        if not subtask or not SUBTASK_RE.match(subtask):
            self._respond(400, b'{"error": "missing or invalid subtask parameter"}')
            return
        try:
            result = fetch_weight(subtask)
            self._respond(200, json.dumps(result).encode())
        except Exception as e:
            print(f"error fetching weight for subtask={subtask!r}: {e}", file=sys.stderr)
            self._respond(502, json.dumps({"error": str(e)}).encode())

    def log_message(self, fmt, *args):
        print(f"{self.address_string()} - {fmt % args}", file=sys.stderr)


def main():
    if not ACCESS_CODE:
        print("BAMBU_ACCESS_CODE is not set", file=sys.stderr)
        raise SystemExit(1)
    print(f"listening on {LISTEN_HOST}:{LISTEN_PORT}, printer={PRINTER_HOST}", file=sys.stderr)
    ThreadingHTTPServer((LISTEN_HOST, LISTEN_PORT), Handler).serve_forever()


if __name__ == "__main__":
    main()

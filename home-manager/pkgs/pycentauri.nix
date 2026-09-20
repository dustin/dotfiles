{ pkgs }:

let
  python = pkgs.python313;
in
pkgs.writeShellApplication {
  name = "centauri";
  runtimeInputs = [ pkgs.uv python ];
  text = ''
    UV_TOOL_DIR="''${UV_TOOL_DIR:-$HOME/.local/share/pycentauri}"
    UV_TOOL_BIN_DIR="''${UV_TOOL_BIN_DIR:-$UV_TOOL_DIR/bin}"
    export UV_TOOL_DIR UV_TOOL_BIN_DIR
    mkdir -p "$UV_TOOL_BIN_DIR"

    if [[ ! -x "$UV_TOOL_BIN_DIR/centauri" ]]; then
      echo "[pycentauri] installing pycentauri into $UV_TOOL_DIR ..." >&2
      uv tool install --python ${python}/bin/python3.13 "pycentauri"
    fi

    exec "$UV_TOOL_BIN_DIR/centauri" "$@"
  '';
}

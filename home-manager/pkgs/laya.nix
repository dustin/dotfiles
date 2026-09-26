{ pkgs }:

let
  python = pkgs.python313;
in
pkgs.writeShellApplication {
  name = "laya-serve";
  runtimeInputs = [ pkgs.uv python ];
  text = ''
    UV_TOOL_DIR="''${UV_TOOL_DIR:-$HOME/.local/share/laya}"
    UV_TOOL_BIN_DIR="''${UV_TOOL_BIN_DIR:-$UV_TOOL_DIR/bin}"
    export UV_TOOL_DIR UV_TOOL_BIN_DIR
    mkdir -p "$UV_TOOL_BIN_DIR"

    if [[ ! -x "$UV_TOOL_BIN_DIR/laya-serve" ]]; then
      echo "[laya] installing laya[serve] into $UV_TOOL_DIR ..." >&2
      uv tool install --python ${python}/bin/python3.13 "laya[serve]"
    fi

    exec "$UV_TOOL_BIN_DIR/laya-serve" "$@"
  '';
}

{ pkgs, headroom }:

pkgs.writeShellApplication {
  name = "headroom-proxy";
  runtimeInputs = [ headroom ];
  text = ''
    export OPENAI_TARGET_API_URL="''${OPENAI_TARGET_API_URL:-http://localhost:11434/v1}"
    export OPENAI_API_KEY="''${OPENAI_API_KEY:-ollama}"
    export HEADROOM_MODE="''${HEADROOM_MODE:-token}"
    export HEADROOM_DISABLE_KOMPRESS="''${HEADROOM_DISABLE_KOMPRESS:-1}"
    export HEADROOM_PORT="''${HEADROOM_PORT:-8787}"
    exec headroom proxy --port "$HEADROOM_PORT"
  '';
}

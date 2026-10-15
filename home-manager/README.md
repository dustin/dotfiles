# Moved

The home-manager configuration now lives in the `home/` directory of
dustin/nixos-configs (private). Switch a machine with:

```sh
home-manager switch --refresh --flake 'git+ssh://git@github.com/dustin/nixos-configs#dustin@<host>'
```

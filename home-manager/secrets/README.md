# Encrypted secrets (sops-nix)

This directory contains sops-encrypted secret files. They are safe to commit to
the public repo because only the age recipients listed in `.sops.yaml` can
decrypt them.

## Files

| File | Decrypted to | Used by |
|------|--------------|---------|
| `nut-password.sops.yaml` | `~/.config/sops-nix/secrets/nut-password` | bee1/bee2 `nuttomqtt` service |
| `aws-credentials.sops.yaml` | `~/.aws/credentials` | all machines (AWS CLI / rclone / papertrails) |
| `rclone-config.sops.yaml` | `~/.config/rclone/rclone.conf` | all machines (rclone, zfstos3, s3bak) |

## Creating a secret for the first time

From `home-manager/` (so `.sops.yaml` is in the current or parent directory):

```sh
cd /Users/dustin/prog/dotfiles/home-manager
sops edit secrets/nut-password.sops.yaml
```

Enter the YAML content with a top-level key matching the secret name:

```yaml
nut-password: YOUR_REAL_NUT_PASSWORD
```

Save and quit. `sops` encrypts the value automatically.

For multi-line files like `aws-credentials` or `rclone-config`, use a literal
block:

```yaml
aws-credentials: |
  [default]
  aws_access_key_id = AKIA...
  aws_secret_access_key = ...
  region = us-west-1
```

## Updating secrets later

```sh
cd /Users/dustin/prog/dotfiles/home-manager
sops edit secrets/nut-password.sops.yaml
```

## Re-encrypting for new recipients

After adding a new machine key to `.sops.yaml`, update all files:

```sh
cd /Users/dustin/prog/dotfiles/home-manager
sops updatekeys secrets/*.sops.yaml
```

## Machine keys

Edit `.sops.yaml` to add new age public keys as you generate them on each
machine. Only add real keys; `sops` will reject invalid placeholders.

Current recipients are in `.sops.yaml`.

## Age key location

`sops` CLI looks for age keys at `~/.config/sops/age/keys.txt` by default, but
this repo configures sops-nix to use the existing key at
`~/.config/age/keys.txt`.

A symlink at the default sops path does **not** work with sops on macOS, so the
repo sets the `SOPS_AGE_KEY_FILE` environment variable via `.envrc` for anyone
using `direnv`. If you enter the `home-manager/` directory and `direnv` is
allowed, `sops edit` / `sops -d` will work automatically.

If you prefer not to use `direnv`, export the variable manually:

```sh
export SOPS_AGE_KEY_FILE="$HOME/.config/age/keys.txt"
```

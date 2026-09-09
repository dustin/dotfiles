{ config, pkgs, lib, hostname, ... }:

{
  # Common packages
  home.packages = with pkgs; [
    sqlite-interactive
    duckdb
    mosh
    ffmpeg
    mtr
    p7zip
    rclone
    jq
    asciinema
    # exiftool
    gnupg
    # watch
    viddy # watch replacement that looks a lot better
    libiconv
    cacert
    # croc # moving stuff
    age # encryption
    minisign # signing stuff
    btop # bee top
    # kitty # terminal graphics?  why not
    bat # show files.  Not really much to do with concatenation
    lsd # ls
    dust # disk usage some thing
    duf # df thing
    fzf # fuzzy finder
    fd # find
    ripgrep # rg
    # bottom # btm top thing
    gping # graphical ping
    procs # ps
    xz
    diff-so-fancy # diff viewer
    zoxide # directory history manager - z cmd
    pueue # at like thing
    rdfind # duplicate file handler
    sops # secret manager
  ];

  home = {
    username      = lib.mkDefault "dustin";
    homeDirectory = lib.mkDefault "/home/dustin";
    stateVersion = "24.11";

    file = {
      ".config/bat/config".text = "--style=plain";
      ".config/fd/config".text = "--hidden\n--no-ignore\n";
    };
  };

  programs = {
    home-manager.enable = true;

    git.enable = true;

    jujutsu = {
      enable = true;
      settings = {
        user = {
          name = "Dustin Sallings";
          email = "dustin@spy.net";
        };

        ui = {
          "default-command" = "mylog";
          editor = "vi";
          pager = "diff-so-fancy";
        };

        aliases = {
          here = [ "b" "m" "--to" "@-" ];
          l = [ "log" "-r" "::" "--limit" "10" ];
          push = [ "git" "push" ];
          clone = [ "git" "clone" "--colocate" ];
          fetch = [ "git" "fetch" ];
          glog = [ "log" "-r" "::@" ];
          mylog = [ "log" "-r" "alias_l()" ];
        };

        "revset-aliases" = {
          "alias_l()" = "ancestors(present(@), 10) | (ancestors(immutable_heads().., 2) & mine()) | present(trunk()) | bookmarks()";
          "alias_ll()" = "alias_l() | ::@";
        };

        "template-aliases" = {
          biglog = ''
            concat(
              committer.timestamp(), " ",
              commit_id.short(), " ",
              author.email(), "\n",
              description, "\n",
              diff.summary())
          '';

          difflog = ''
            concat(
              committer.timestamp(), " ",
              commit_id.short(), " ",
              author.email(), "\n",
              description, "\n",
              diff.git())
          '';
        };

        templates = {
          log = ''
            builtin_log_compact
            ++ if(
                !description && !empty,
                diff.summary(),
            )
            ++ if(
                conflict,
                self.files(
                    "all()"
                ).filter(
                    |file| file.conflict()
                ).map(
                    |file| label(
                        "conflict",
                        concat("conflict ", file.path())
                    )
                ).join("\n")
            )
          '';
        };
      };
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    tmux = {
      enable = true;
      clock24 = true;
      historyLimit = 50000;
      terminal = "screen-256color";
      extraConfig = ''
        set-environment -gu ATUIN_TMUX_POPUP
        set -g status-right '#(echo $USER) @ #h %a %Y-%m-%d %H:%M'
        setw -g allow-rename on
      '';
    };

    atuin = {
      enable = true;
      enableZshIntegration = true;
      flags = [ "--disable-up-arrow" ];
      settings = {
        style = "compact";
        enter_accept = false;
        inline_height = 30;
        filter_mode = "host";
        auto_sync = true;
        sync_address = "http://bee2:8888";
        sync_frequency = "5m";
        search = {
          filters = [ "host" "directory" "session" "global" ];
        };
        keys = {
          scroll_exits = false;
        };
        tmux = {
          enabled = true;
          width = "80%";
          height = "60%";
        };
        dotfiles = {
          enabled = true;
        };
        show_help = true;
        show_tabs = false;
        history_filter = [
          "^cd$"
          "^cd "
          "^ls$"
          "^ll$"
          "^pwd$"
          "^clear$"
          "^exit$"
          "^z$"
          "^z "
          "^w$"
        ];
      };
    };


    zsh = {
      enable = true;
      envExtra = ''
        if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
            . "$HOME/.nix-profile/etc/profile.d/nix.sh"
        elif [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
            . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        fi
      '';
      initContent = ''
         autoload -Uz select-word-style
         select-word-style bash
         setopt no_share_history
         unsetopt share_history
      '';
      shellAliases = {
        ls = "lsd";
        ll = "lsd -Al --date=relative";
        watch = "viddy";
	    pu = "pueue";
        "hm-switch" = ''
          home-manager switch --refresh --flake 'github:dustin/dotfiles?dir=home-manager#dustin@${hostname}'
        '';

      };
    };


    zsh.oh-my-zsh = {
      enable = true;
      plugins = [ "zoxide" ];
    };
  };
}
